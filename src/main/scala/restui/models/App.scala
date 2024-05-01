package restui.models
import java.io.{File, PrintWriter}
import java.time.Instant

import scala.io.Source
import scala.reflect.ClassTag

import io.github.iltotore.iron.{refineEither, refineOption}
import monocle.syntax.all._
import restui.models
import restui.views.{StatefulList, StatefulTab, StatefulTable}
import tui.crossterm.{CrosstermJni, Event}
import tui.{Frame, Terminal}
import zio._
import zio.http._

private val EnvEditor = "RESTUI_EDITOR"
private val EnvCopy = "RESTUI_COPY"

case class App(
    val terminal: Terminal,
    val render: (f: Frame, app: App) => Unit,
    var currentLocation: Location,
    var collectionList: StatefulList[Collection],
    var requestTab: Tabs[models.Request],
    var editTextModal: EditTextModal = EditTextModal.init,
    var editValueModal: EditValueModal = EditValueModal.init,
    var requestView: RequestView,
    var env: Option[restui.Env],
    var responseView: Option[ResponseView],
    var message: Option[Message] = None,
    var configPath: Option[String] = None,
    private var loading: Boolean = false
):
  def currentCollection: models.Collection = collectionList.current.get
  def currentRequest: models.Request = currentCollection.requests.lift(requestTab.index).get
  def isLoading = loading

  def getRequestTab(implicit ev: ClassTag[models.Request]) =
    val requests = collectionList.current.map(_.requests).getOrElse(Array())
    requestTab.update(requests)

  def update(
      collectionList: Option[StatefulList[Collection]] = None,
      currentLocation: Option[Location] = None,
      loading: Option[Boolean] = None,
      requestTab: Option[Tabs[models.Request]] = None,
      requestView: Option[RequestView] = None,
      responseView: Option[Option[ResponseView]] = None
  ) =
    this.collectionList = collectionList
      .getOrElse(this.collectionList)
    this.requestTab = requestTab.getOrElse(this.requestTab)
    this.loading = loading.getOrElse(this.loading)
    this.currentLocation = currentLocation.getOrElse(this.currentLocation)
    this.requestView = requestView.getOrElse(this.requestView)
    responseView.map(v => this.responseView = v)
    this

  def updateCollectionList(collection: models.Collection) =
    update(
      collectionList = Some(
        collectionList.update(
          collectionList.items.map(item =>
            if (item.id == collection.id)
              collection
            else item
          )
        )
      )
    ).update(
      requestTab = Some(
        if (currentCollection.id == collection.id)
          requestTab.update(collection.requests)
        else requestTab
      )
    ).update(requestView = Some(requestView.update(currentRequest)))

  def run(jni: CrosstermJni, tickRate: Duration) =
    val uiLoop = for {
      lastTick <- ZIO.succeed(Instant.now())
      event <- draw(jni, tickRate, lastTick)
      exitCode <-
        ZIO.whenCase((editTextModal.isOpen, editValueModal.isOpen)) {
          case (true, _) =>
            event.map(processEdiTextInput(_)).getOrElse(ZIO.unit)
          case (_, true) =>
            event.map(processEdiKeyValueInput(_)).getOrElse(ZIO.unit)
          case _ =>
            event
              .map(
                processInput(_)
                  .catchAll(e =>
                    for {
                      _ <- e match
                        case e @ (Error.IOError(_) | Error.ConfigNotFound(_) | Error.EnvNotFound(_)) =>
                          ZIO.succeed(showMessage(Message.Error(e.toString())))
                        case e: Throwable => ZIO.succeed(showMessage(Message.Error(e.getMessage())))
                        case _            => ZIO.unit
                      _ <- ZIO.succeed(update(responseView = Some(None), loading = Some(false)))
                    } yield ()
                  )
                  .provide(Client.default, Scope.default)
              )
              .getOrElse(ZIO.unit)
        }
    } yield exitCode.getOrElse(())

    uiLoop
      .repeatWhile {
        _ match
          case Some(Exit.Success(_)) => false
          case Some(Exit.Failure(_)) => false
          case _                     => true
      }

  private def showEditModal(
      title: String = "",
      text: String = "",
      location: models.Location = models.Location.Collection,
      state: EditState = EditState.Add,
      validate: ValidateText
  ) =
    this.editTextModal = this.editTextModal.open(title, text, location, state, validate)
    this.editValueModal = this.editValueModal.close

  private def showEditValueModal(
      nameTitle: String = "",
      valueTitle: String = "",
      editValue: models.EditValue = models.EditValue(),
      location: models.Location = models.Location.Collection,
      state: models.EditState = models.EditState.Add,
      validateName: models.ValidateEditValue,
      validateValue: models.ValidateEditValue
  ) =
    this.editValueModal = this.editValueModal.open(
      nameTitle = nameTitle,
      valueTitle = valueTitle,
      editValue = editValue,
      location = location,
      state = state,
      validateName = validateName,
      validateValue = validateValue
    )
    this.editTextModal = this.editTextModal.close

  private def closeModal =
    this.editTextModal = this.editTextModal.close
    this.editValueModal = this.editValueModal.close

  private def inputText(input: Char) =
    if (this.editTextModal.isOpen)
      this.editTextModal = editTextModal.input(editTextModal.text + input)
    else if (this.editValueModal.isOpen)
      this.editValueModal = editValueModal.input(editValueModal.currentText + input)

  private def deleteText =
    if (this.editTextModal.isOpen)
      this.editTextModal = this.editTextModal.delete
    else if (this.editValueModal.isOpen)
      this.editValueModal = this.editValueModal.delete

  private def inputMove =
    if (this.editValueModal.isOpen)
      this.editValueModal = this.editValueModal.move

  private def prevCursor =
    update(
      currentLocation = Some(
        currentLocation.prev(
          hasCollection = !collectionList.items.isEmpty,
          hasRequest = !requestTab.isEmpty,
          hasResponse = responseView.isDefined
        )
      )
    )
    updateCollectionList(currentCollection)
    clearMessage
    this

  private def nextCursor =
    update(
      currentLocation = Some(
        currentLocation.next(
          hasCollection = !collectionList.items.isEmpty,
          hasRequest = !requestTab.isEmpty,
          hasResponse = responseView.isDefined
        )
      )
    )
    updateCollectionList(currentCollection)
    clearMessage
    this

  private def showMessage(message: Message) =
    this.message = Some(message)
    this.loading = false

  private def clearMessage =
    this.message = None
    this.loading = false

  private def refresh = ZIO.succeed(terminal.draw(f => render(f, this)))

  private def draw(jni: CrosstermJni, tickRate: Duration, lastTick: Instant) =
    for {
      _ <- ZIO.succeed(terminal.draw(f => render(f, this)))
      elapsed <- ZIO.succeed(java.time.Duration.between(lastTick, java.time.Instant.now()))
      timeout <- ZIO.succeed(tickRate.minus(elapsed))
      timeoutDuration <- ZIO.succeed(new tui.crossterm.Duration(timeout.getSeconds(), timeout.getNano))
      key <- ZIO.when(jni.poll(timeoutDuration))(ZIO.succeed(jni.read()))
    } yield key

  private def processInput(event: Event) =
    ZIO.whenCase(event) {
      case key: tui.crossterm.Event.Key =>
        key.keyEvent.code match {
          case _: tui.crossterm.KeyCode.Tab     => ZIO.succeed(nextCursor)
          case _: tui.crossterm.KeyCode.BackTab => ZIO.succeed(prevCursor)
          case char: tui.crossterm.KeyCode.Char if char.c() == 'a' =>
            currentLocation match
              case Location.Collection =>
                ZIO.succeed(
                  showEditModal(
                    title = "Add Collection",
                    location = Location.Collection,
                    validate = (text) => text.refineEither[models.CollectionName].map(v => v.toString)
                  )
                )
              case Location.Request =>
                ZIO.succeed(
                  showEditModal(
                    title = "Add Request",
                    location = Location.Request,
                    validate = (text) => text.refineEither[models.RequestName].map(v => v.toString)
                  )
                )
              case Location.Param =>
                requestView.currentParams match
                  case models.QueryParams(_) =>
                    ZIO.succeed(
                      showEditValueModal(
                        nameTitle = requestView.paramName,
                        valueTitle = requestView.valueName,
                        location = Location.Param,
                        validateName = (name) => name.refineEither[models.QueryName].map(v => v.toString),
                        validateValue = (value) => value.refineEither[models.QueryValue].map(v => v.toString)
                      )
                    )
                  case models.Headers(_) =>
                    ZIO.succeed(
                      showEditValueModal(
                        nameTitle = requestView.paramName,
                        valueTitle = requestView.valueName,
                        location = Location.Param,
                        validateName = (name) => name.refineEither[models.HeaderKey].map(v => v.toString),
                        validateValue = (value) => value.refineEither[models.HeaderValue].map(v => v.toString)
                      )
                    )
                  case models.Body(BodyType.UrlEncodedForm(_)) | models.Body(BodyType.MultipartForm(_)) =>
                    ZIO.succeed(
                      showEditValueModal(
                        nameTitle = requestView.paramName,
                        valueTitle = requestView.valueName,
                        location = Location.Param,
                        validateName = (name) => name.refineEither[models.FormKey].map(v => v.toString),
                        validateValue = (value) => value.refineEither[models.FormValue].map(v => v.toString)
                      )
                    )
                  case _ => ZIO.unit
              case _ => ZIO.unit
          case char: tui.crossterm.KeyCode.Char
              if char.c() == 'c' && (key.keyEvent
                .modifiers()
                .bits & tui.crossterm.KeyModifiers.CONTROL) == tui.crossterm.KeyModifiers.CONTROL =>
            ZIO.succeed(0).exit
          case char: tui.crossterm.KeyCode.Char if char.c() == 'c' =>
            currentLocation match
              case Location.Request | Location.Method | Location.RequestUrl | Location.Param =>
                for {
                  _ <- ZIO
                    .attempt(copyToClipboard(requestView.request.toCurlCommand(env)))
                    .mapError(e =>
                      e match
                        case e: Throwable => Error.IOError(e)
                    )
                  _ <- ZIO.succeed(showMessage(Message.Info("Copy curl command to clipboard.")))
                } yield ()
              case Location.Response =>
                for {
                  current <- ZIO.fromOption(responseView.flatMap(_.current))
                  currentHeader <- ZIO.fromOption(responseView.flatMap(_.currentHeader))
                  _ <- ZIO
                    .attempt(current match
                      case models.ResponseBody(_, body, _, _) =>
                        copyToClipboard(body)
                      case models.ResponseHeaders(_) =>
                        copyToClipboard(s"${currentHeader.key}:${currentHeader.value}")
                    )
                    .mapError(e =>
                      e match
                        case e: Throwable => Error.IOError(e)
                    )
                  _ <- ZIO.succeed(showMessage(Message.Info("Copy to clipboard.")))
                } yield ()
              case _ => ZIO.unit
          case char: tui.crossterm.KeyCode.Char if char.c() == 'e' =>
            currentLocation match
              case Location.Collection =>
                ZIO.succeed(
                  showEditModal(
                    title = "Edit Collection",
                    text = currentCollection.name,
                    location = Location.Collection,
                    state = models.EditState.Edit,
                    validate = (text) => text.refineEither[models.CollectionName].map(v => v.toString)
                  )
                )
              case Location.Request =>
                ZIO.succeed(
                  showEditModal(
                    title = "Edit Request",
                    text = currentRequest.name,
                    location = Location.Request,
                    state = models.EditState.Edit,
                    validate = (text) => text.refineEither[models.RequestName].map(v => v.toString)
                  )
                )
              case Location.RequestUrl =>
                ZIO.succeed(
                  showEditModal(
                    title = "Edit URL",
                    text = currentRequest.url,
                    location = Location.RequestUrl,
                    state = models.EditState.Edit,
                    validate = (text) => text.refineEither[models.RequestUrl].map(v => v.toString)
                  )
                )
              case Location.Param =>
                for {
                  _ <- ZIO.whenCase(requestView.selectedParam) {
                    case Some(models.Query(_, key, value)) =>
                      ZIO.succeed(
                        showEditValueModal(
                          nameTitle = requestView.paramName,
                          valueTitle = requestView.valueName,
                          location = Location.Param,
                          state = models.EditState.Edit,
                          editValue = models.EditValue(key, value),
                          validateName = (name) => name.refineEither[models.QueryName].map(v => v.toString),
                          validateValue = (value) => value.refineEither[models.QueryValue].map(v => v.toString)
                        )
                      )
                    case Some(models.FormField(_, key, value)) =>
                      ZIO.succeed(
                        showEditValueModal(
                          nameTitle = requestView.paramName,
                          valueTitle = requestView.valueName,
                          location = Location.Param,
                          state = models.EditState.Edit,
                          editValue = models.EditValue(key, value),
                          validateName = (name) => name.refineEither[models.FormKey].map(v => v.toString),
                          validateValue = (value) => value.refineEither[models.FormValue].map(v => v.toString)
                        )
                      )
                    case Some(Auth(AuthType.BearerToken(_))) =>
                      ZIO.succeed(
                        showEditModal(
                          title = "Edit Bearer Token",
                          text = "",
                          location = Location.Param,
                          state = EditState.Edit,
                          validate = (text) => Right(text)
                        )
                      )
                    case Some(Auth(AuthType.Basic(userName, password))) =>
                      ZIO.succeed(
                        showEditValueModal(
                          nameTitle = requestView.paramName,
                          valueTitle = requestView.valueName,
                          location = Location.Param,
                          state = models.EditState.Edit,
                          editValue = models.EditValue(userName, password),
                          validateName = (name) => Right(name),
                          validateValue = (value) => Right(value)
                        )
                      )
                    case Some(models.Header(_, name, value)) =>
                      ZIO.succeed(
                        showEditValueModal(
                          nameTitle = requestView.paramName,
                          valueTitle = requestView.valueName,
                          location = Location.Param,
                          state = models.EditState.Edit,
                          editValue = models.EditValue(name, value),
                          validateName = (name) => name.refineEither[models.HeaderKey].map(v => v.toString),
                          validateValue = (value) => value.refineEither[models.HeaderValue].map(v => v.toString)
                        )
                      )
                    case Some(BodyType.Raw(raw)) =>
                      ZIO
                        .scoped {
                          for {
                            tempFile <- ZIO.attempt(File.createTempFile("restui_body", ".txt"))
                            _ <- ZIO.succeed(tempFile.deleteOnExit())
                            writeFile <- ZIO.fromAutoCloseable(ZIO.attempt(new PrintWriter(tempFile)))
                            _ <- ZIO.attempt(writeFile.write(raw.toString))
                            _ <- ZIO.attempt(writeFile.close())
                            _ <- ZIO.attempt(
                              new ProcessBuilder(sys.env.getOrElse(EnvEditor, "vim"), tempFile.getAbsolutePath())
                                .redirectOutput(ProcessBuilder.Redirect.INHERIT)
                                .redirectError(ProcessBuilder.Redirect.INHERIT)
                                .redirectInput(ProcessBuilder.Redirect.INHERIT)
                                .start()
                                .waitFor()
                            )
                            readFile <- ZIO.fromAutoCloseable(ZIO.attempt(Source.fromFile(tempFile.getAbsolutePath())))
                            updatedCollection <- ZIO.succeed(
                              currentCollection.updateRequest(
                                requestView.request.updateBodyRaw(readFile.getLines.mkString("\n"))
                              )
                            )
                            _ <- ZIO.succeed(updateCollectionList(updatedCollection))
                            _ <- ZIO.succeed(closeModal)
                          } yield ()
                        }
                        .mapError(e =>
                          e match
                            case e: Throwable => Error.IOError(e)
                        )
                    case _ => ZIO.unit
                  }
                } yield ()
              case _ => ZIO.unit
          case char: tui.crossterm.KeyCode.Char
              if char.c() == 'd' && (key.keyEvent
                .modifiers()
                .bits & tui.crossterm.KeyModifiers.CONTROL) == tui.crossterm.KeyModifiers.CONTROL =>
            currentLocation match
              case Location.Collection if collectionList.items.length > 1 =>
                ZIO.succeed(
                  update(collectionList =
                    Some(collectionList.update(collectionList.items.filter(item => item.id != currentCollection.id)))
                  )
                )
              case Location.Request if currentCollection.requests.length > 1 =>
                for {
                  _ <- ZIO.succeed(updateCollectionList(currentCollection.copy(modified = true).deleteRequest(currentRequest)))
                  _ <- ZIO.succeed(closeModal)
                } yield ()
              case Location.Param =>
                for {
                  _ <- ZIO.succeed(
                    updateCollectionList(currentCollection.updateRequest(requestView.deleteSelectedQuery.request))
                  )
                  _ <- ZIO.succeed(closeModal)
                } yield ()
              case _ => ZIO.unit
          case char: tui.crossterm.KeyCode.Char if char.c() == 'd' =>
            currentLocation match
              case Location.Request if currentCollection.requests.length > 1 =>
                for {
                  _ <- ZIO.succeed(updateCollectionList(currentCollection.addRequest(currentRequest.duplicate)))
                  _ <- ZIO.succeed(closeModal)
                } yield ()
              case _ => ZIO.unit
          case char: tui.crossterm.KeyCode.Char if char.c() == 's' =>
            for {
              _ <- ZIO.succeed(
                update(collectionList =
                  Some(
                    collectionList.update(
                      collectionList.items.map(collection =>
                        collection.copy(modified = false, requests = collection.requests.map(_.unmodified))
                      )
                    )
                  )
                )
              )
              _ <- models.Config(configPath).write(collectionList.items)
              _ <- ZIO.succeed(showMessage(Message.Info("Collections has been saved.")))
            } yield ()

          case char: tui.crossterm.KeyCode.Char
              if char.c() == 'l' && (key.keyEvent
                .modifiers()
                .bits & tui.crossterm.KeyModifiers.CONTROL) == tui.crossterm.KeyModifiers.CONTROL =>
            for {
              items <- models.Config(configPath).read
              _ <- ZIO.succeed(update(collectionList = Some(collectionList.update(items))))
              _ <- ZIO.succeed(showMessage(Message.Info("Reload collections.")))
            } yield ()

          case char: tui.crossterm.KeyCode.Char if char.c() == 'q' =>
            ZIO.succeed(0).exit
          case char: tui.crossterm.KeyCode.Char if char.c() == 'm' =>
            currentLocation match
              case Location.Method =>
                ZIO.succeed(updateCollectionList(currentCollection.updateRequest(requestView.changeMethod.request)))
              case Location.Param if requestView.isAuthTabSelected =>
                ZIO.succeed(updateCollectionList(currentCollection.updateRequest(requestView.changeAuth.request)))
              case Location.Param if requestView.isBodyTabSelected =>
                ZIO.succeed(updateCollectionList(currentCollection.updateRequest(requestView.changeBody.request)))
              case _ => ZIO.unit
          case char: tui.crossterm.KeyCode.Char
              if char.c() == 'r' && (key.keyEvent
                .modifiers()
                .bits & tui.crossterm.KeyModifiers.CONTROL) == tui.crossterm.KeyModifiers.CONTROL =>
            for {
              _ <- ZIO.succeed(update(responseView = Some(None), loading = Some(true)))
              _ <- refresh
              res <- currentRequest.send(env)
              _ <- ZIO.succeed(
                update(
                  responseView = Some(
                    Some(
                      ResponseView(
                        tab = StatefulTab(tabs = Array(res._1, res._2)),
                        body = StatefulTable(items = res._1.toString.split("\n")),
                        header = StatefulTable(items = res._2.headers.headers)
                      )
                    )
                  ),
                  loading = Some(false)
                )
              )
            } yield res
          case _: tui.crossterm.KeyCode.Left =>
            currentLocation match
              case Location.Collection => ZIO.unit
              case Location.Method     => ZIO.succeed(prevCursor.prevCursor)
              case Location.Request if requestTab.index > 0 =>
                ZIO.succeed(
                  update(requestTab = Some(requestTab.previous)).update(requestView = Some(models.RequestView(currentRequest)))
                )
              case Location.Param => ZIO.succeed(update(requestView = Some(requestView.previousParamTab)))
              case Location.Response if responseView.map(_.tab.index).getOrElse(0) != 0 =>
                ZIO.succeed(update(responseView = Some(responseView.map(_.prevTab))))
              case _ => ZIO.succeed(prevCursor)
          case _: tui.crossterm.KeyCode.Down =>
            currentLocation match
              case Location.Collection =>
                ZIO.succeed(update(collectionList = Some(collectionList.next)).updateCollectionList(currentCollection))
              case Location.Param  => ZIO.succeed(update(requestView = Some(requestView.nextParam)))
              case Location.Method => ZIO.succeed(nextCursor.nextCursor)
              case Location.Response =>
                ZIO.succeed(responseView.flatMap(_.current) match
                  case Some(models.ResponseBody(_, _, _, _)) => update(responseView = Some(responseView.map(_.nextBody)))
                  case Some(models.ResponseHeaders(_))       => update(responseView = Some(responseView.map(_.nextHeader)))
                  case _                                     => ()
                )
              case _ => ZIO.succeed(nextCursor)
          case _: tui.crossterm.KeyCode.Up =>
            currentLocation match
              case Location.Collection =>
                ZIO.succeed(update(collectionList = Some(collectionList.previous)).updateCollectionList(currentCollection))
              case Location.Request    => ZIO.unit
              case Location.RequestUrl => ZIO.succeed(prevCursor.prevCursor)
              case Location.Param if !requestView.isFirstRowSelected =>
                ZIO.succeed(update(requestView = Some(requestView.previousParam)))
              case Location.Response =>
                ZIO.succeed(responseView.flatMap(_.current) match
                  case Some(models.ResponseBody(_, _, _, _)) => update(responseView = Some(responseView.map(_.prevBody)))
                  case Some(models.ResponseHeaders(_))       => update(responseView = Some(responseView.map(_.prevHeader)))
                  case _                                     => ()
                )
              case _ => ZIO.succeed(prevCursor)
          case _: tui.crossterm.KeyCode.Right =>
            currentLocation match
              case Location.Request =>
                ZIO.succeed(
                  update(requestTab = Some(requestTab.next)).update(requestView = Some(models.RequestView(currentRequest)))
                )
              case Location.Param if !requestView.isHeaderTabSelected =>
                ZIO.succeed(update(requestView = Some(requestView.nextParamTab)))
              case Location.Response =>
                ZIO.succeed(update(responseView = Some(responseView.map(_.nextTab))))
              case _ => ZIO.succeed(nextCursor)
          case _ => ZIO.unit
        }
      case _ => ZIO.unit
    }

  private def processEdiTextInput(event: Event) =
    for {
      _ <- ZIO.whenCase(event) { case key: tui.crossterm.Event.Key =>
        key.keyEvent.code match
          case char: tui.crossterm.KeyCode.Char
              if char.c() == 'c' && (key.keyEvent
                .modifiers()
                .bits & tui.crossterm.KeyModifiers.CONTROL) == tui.crossterm.KeyModifiers.CONTROL =>
            ZIO.succeed(0).exit
          case char: tui.crossterm.KeyCode.Char   => ZIO.succeed(inputText(char.c()))
          case _: tui.crossterm.KeyCode.Esc       => ZIO.succeed(closeModal)
          case _: tui.crossterm.KeyCode.Backspace => ZIO.succeed(deleteText)
          case _: tui.crossterm.KeyCode.Enter if editTextModal.isValid =>
            ZIO.whenCase(editTextModal.location) {
              case Location.Collection if editTextModal.state.isAdd =>
                for {
                  collectionName <- ZIO.fromOption(editTextModal.text.refineOption[models.CollectionName])
                  collectionId <- ZIO.fromOption(java.util.UUID.randomUUID.toString.refineOption[models.Id])
                  _ <- ZIO.succeed(
                    update(collectionList =
                      Some(
                        collectionList.add(
                          models
                            .Collection(collectionId, collectionName)
                            .addRequest(models.Request.create())
                        )
                      ).map(_.last)
                    )
                  )
                  _ <- ZIO.succeed(closeModal)
                } yield ()
              case Location.Collection if editTextModal.state.isEdit =>
                for {
                  collectionName <- ZIO.fromOption(editTextModal.text.refineOption[models.CollectionName])
                  _ <- ZIO.succeed(updateCollectionList(currentCollection.changeName(collectionName)))
                  _ <- ZIO.succeed(closeModal)
                } yield ()
              case Location.Request if editTextModal.state.isAdd =>
                for {
                  _ <- ZIO.succeed(
                    updateCollectionList(
                      currentCollection.addRequest(models.Request.create(editTextModal.text).copy(modified = true))
                    )
                  )
                  _ <- ZIO.succeed(
                    update(requestTab = Some(requestTab.last)).update(requestView = Some(models.RequestView(currentRequest)))
                  )
                  _ <- ZIO.succeed(closeModal)
                } yield ()
              case Location.Request if editTextModal.state.isEdit =>
                for {
                  text <- ZIO.fromOption(editTextModal.text.refineOption[models.RequestName])
                  _ <- ZIO.succeed(updateCollectionList(currentCollection.updateRequest(currentRequest.changeName(text))))
                  _ <- ZIO.succeed(closeModal)
                } yield ()
              case Location.RequestUrl if editTextModal.state.isEdit =>
                for {
                  updatedRequest <- ZIO.fromOption(
                    editTextModal.text
                      .refineOption[models.RequestUrl]
                      .map(
                        currentRequest.changeUrl(_)
                      )
                  )
                  _ <- ZIO.succeed(updateCollectionList(currentCollection.updateRequest(updatedRequest)))
                  _ <- ZIO.succeed(closeModal)
                } yield ()
              case Location.Param if editTextModal.state.isEdit && requestView.isAuthTabSelected =>
                for {
                  _ <- ZIO.succeed(
                    updateCollectionList(
                      currentCollection.updateRequest(
                        requestView.request.changeAuth(Auth(AuthType.BearerToken(editTextModal.text)))
                      )
                    )
                  )
                  _ <- ZIO.succeed(closeModal)
                } yield ()
              case _ => ZIO.unit
            }
          case _ => ZIO.unit
      }
    } yield ()

  private def processEdiKeyValueInput(
      event: Event
  ) =
    for {
      _ <- ZIO.whenCase(event) { case key: tui.crossterm.Event.Key =>
        key.keyEvent.code match
          case char: tui.crossterm.KeyCode.Char
              if char.c() == 'c' && (key.keyEvent
                .modifiers()
                .bits & tui.crossterm.KeyModifiers.CONTROL) == tui.crossterm.KeyModifiers.CONTROL =>
            ZIO.succeed(0).exit
          case char: tui.crossterm.KeyCode.Char   => ZIO.succeed(inputText(char.c()))
          case _: tui.crossterm.KeyCode.Backspace => ZIO.succeed(deleteText)
          case _: tui.crossterm.KeyCode.Esc       => ZIO.succeed(closeModal)
          case _: tui.crossterm.KeyCode.Down | _: tui.crossterm.KeyCode.Up | _: tui.crossterm.KeyCode.Tab |
              _: tui.crossterm.KeyCode.BackTab =>
            ZIO.succeed(inputMove)
          case _: tui.crossterm.KeyCode.Enter if editValueModal.isValid =>
            for {
              currentCollection <- ZIO.fromOption(collectionList.current)
              _ <- ZIO.whenCase(editValueModal.location) {
                case (Location.Param) if editValueModal.state.isAdd =>
                  for {
                    _ <- ZIO.succeed(
                      updateCollectionList(
                        currentCollection.updateRequest(
                          requestView.addParam(editValueModal.editValue.name, editValueModal.editValue.value).request
                        )
                      )
                    )
                    _ <- ZIO.succeed(closeModal)
                  } yield ()
                case (Location.Param) if editValueModal.state.isEdit =>
                  for {
                    _ <- ZIO.succeed(
                      updateCollectionList(
                        currentCollection.updateRequest(
                          requestView.updateParam(editValueModal.editValue.name, editValueModal.editValue.value).request
                        )
                      )
                    )
                    _ <- ZIO.succeed(closeModal)
                  } yield ()
                case _ => ZIO.unit
              }
            } yield ()
          case _ => ZIO.unit
      }
    } yield ()

private def copyToClipboard(text: String) =
  val pbcopy = sys.env.getOrElse(EnvCopy, "pbcopy")
  val process = new ProcessBuilder(pbcopy)
    .redirectOutput(ProcessBuilder.Redirect.INHERIT)
    .redirectError(ProcessBuilder.Redirect.INHERIT)
    .start()
  val writer = new java.io.PrintWriter(process.getOutputStream)
  writer.println(text)
  writer.close()
  process.waitFor()
