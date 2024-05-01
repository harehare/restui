package restui.models
import restui.views.StatefulTable

case class RequestView(
    var request: Request,
    private val paramTab: Tabs[Params],
    private val header: StatefulTable[Header],
    private val query: StatefulTable[Query],
    private val _bodyRaw: StatefulTable[String],
    private val _bodyUrlEncodedForm: StatefulTable[FormField],
    private val _bodyMultipartForm: StatefulTable[FormField]
):
  def addParam(key: String, value: String) =
    paramTab.current match
      case Some(QueryParams(_)) =>
        Query(
          key = key,
          value = value
        )
          .map(query => copy(request = request.addQuery(query)))
          .getOrElse(this)
      case Some(Body(_)) =>
        request.body match
          case Body(BodyType.UrlEncodedForm(_)) =>
            FormField(
              key = key,
              value = value
            )
              .map(formField => copy(request = request.addFormField(formField)))
              .getOrElse(this)
          case Body(BodyType.MultipartForm(_)) =>
            FormField(
              key = key,
              value = value
            )
              .map(formField => copy(request = request.addFormField(formField)))
              .getOrElse(this)
          case _ => this
      case Some(Headers(_)) =>
        Header(
          name = key,
          value = value
        )
          .map(header => copy(request = request.addHeader(header)))
          .getOrElse(this)
      case _ => this

  def updateParam(key: String, value: String) =
    (paramTab.current match
      case Some(QueryParams(_)) =>
        for {
          current <- query.current
          query <- Query(
            id = current.id,
            key = key,
            value = value
          )
          updatedRequest <- Some(request.updateQuery(query))
        } yield copy(request = updatedRequest)
      case Some(Body(bodyType)) =>
        bodyType match
          case BodyType.UrlEncodedForm(_) =>
            for {
              current <- _bodyUrlEncodedForm.current
              field <- FormField(
                id = current.id,
                key = key,
                value = value
              )
              updatedRequest <- Some(request.updateFormField(field))
            } yield copy(request = updatedRequest)
          case BodyType.MultipartForm(_) =>
            for {
              current <- _bodyUrlEncodedForm.current
              field <- FormField(
                id = current.id,
                key = key,
                value = value
              )
              updatedRequest <- Some(request.updateFormField(field))
            } yield copy(request = updatedRequest)
          case _ => Some(this)
      case Some(Auth(auth)) =>
        auth match
          case AuthType.Basic(_, _) =>
            Some(copy(request = request.changeAuth(Auth(AuthType.Basic(userName = key, password = value)))))
          case _ => Some(this)
      case Some(Headers(_)) =>
        for {
          current <- header.current
          header <- Header(
            id = current.id,
            name = key,
            value = value
          )
          updatedRequest <- Some(request.updateHeader(header))
        } yield copy(request = updatedRequest)

      case _ => Some(this)
    ).getOrElse(this)

  def changeBody =
    copy(request =
      request.changeBody(
        request.body match
          case Body(BodyType.NoBody) => Body(BodyType.Raw(_bodyRaw.items.mkString("\n")))
          case Body(BodyType.Raw(_)) =>
            Body(BodyType.UrlEncodedForm(Form(_bodyUrlEncodedForm.items)))
          case Body(BodyType.UrlEncodedForm(_)) =>
            Body(BodyType.MultipartForm(Form(_bodyUrlEncodedForm.items)))
          case Body(BodyType.MultipartForm(_)) => Body(BodyType.NoBody)
      )
    )

  def changeMethod =
    copy(request =
      request.changeMethod(
        request.method match {
          case Method.Get    => Method.Post
          case Method.Post   => Method.Put
          case Method.Put    => Method.Patch
          case Method.Patch  => Method.Delete
          case Method.Delete => Method.Head
          case Method.Head   => Method.Get
        }
      )
    )

  def changeAuth =
    copy(request =
      request.changeAuth(
        Auth(request.auth match
          case Auth(AuthType.NoAuth)         => AuthType.Basic("", "")
          case Auth(AuthType.Basic(_, _))    => AuthType.BearerToken("")
          case Auth(AuthType.BearerToken(_)) => AuthType.NoAuth
        )
      )
    )

  def currentParams: Params =
    paramTab.current match
      case Some(QueryParams(_)) => request.queryParams
      case Some(Body(_))        => request.body
      case Some(Auth(_))        => request.auth
      case Some(Headers(_))     => request.headers
      case _                    => request.queryParams

  def selectedParam =
    paramTab.current match
      case Some(QueryParams(_)) => query.current
      case Some(Body(bodyType)) =>
        bodyType match
          case BodyType.Raw(raw)          => Some(BodyType.Raw(raw))
          case BodyType.UrlEncodedForm(_) => _bodyUrlEncodedForm.current
          case BodyType.MultipartForm(_)  => _bodyMultipartForm.current
          case _                          => None
      case Some(Auth(_))    => Some(request.auth)
      case Some(Headers(_)) => header.current
      case _                => query.current

  def deleteSelectedQuery =
    copy(request = paramTab.current match
      case Some(QueryParams(_)) => query.current.map(request.deleteQuery(_)).getOrElse(request)
      case Some(Body(bodyType)) =>
        bodyType match
          case BodyType.Raw(_) => request
          case BodyType.UrlEncodedForm(_) =>
            _bodyUrlEncodedForm.current.map(request.deleteFormField(_)).getOrElse(request)
          case BodyType.MultipartForm(_) =>
            _bodyMultipartForm.current.map(request.deleteFormField(_)).getOrElse(request)
          case _ => request
      case Some(Headers(_)) => header.current.map(request.deleteHeader(_)).getOrElse(request)
      case _                => request
    )

  def bodyRaw(req: Request = request) =
    req.body match
      case Body(BodyType.Raw(rawText)) => Some(_bodyRaw.update(rawText.split("\n")))
      case _                           => None

  def bodyUrlEncodedForm(req: Request = request) =
    req.body match
      case Body(BodyType.UrlEncodedForm(form)) => Some(_bodyUrlEncodedForm.update(form.fields))
      case _                                   => None

  def bodyMultiPartForm(req: Request = request) =
    req.body match
      case Body(BodyType.MultipartForm(form)) => Some(_bodyUrlEncodedForm.update(form.fields))
      case _                                  => None

  def paramName =
    paramTab.current match
      case Some(QueryParams(_)) => "Query Key"
      case Some(Body(bodyType)) =>
        bodyType match
          case BodyType.Raw(_)            => ""
          case BodyType.UrlEncodedForm(_) => "Form URL Encoded Key"
          case BodyType.MultipartForm(_)  => "Multipart Form Key"
          case _                          => ""
      case Some(Auth(auth)) =>
        auth match
          case AuthType.Basic(_, _) => "User Name"
          case _                    => ""
      case Some(Headers(_)) => "Header Key"
      case None             => "Query Key"

  def valueName =
    paramTab.current match
      case Some(QueryParams(_)) => "Query Value"
      case Some(Body(bodyType)) =>
        bodyType match
          case BodyType.Raw(_)            => ""
          case BodyType.UrlEncodedForm(_) => "Form URL Encoded Value"
          case BodyType.MultipartForm(_)  => "Multipart Form Value"
          case _                          => ""
      case Some(Auth(auth)) =>
        auth match
          case AuthType.Basic(_, _) => "Password"
          case _                    => ""
      case Some(Headers(_)) => "Header Value"
      case None             => "Query Value"

  def previousParamTab =
    copy(paramTab = paramTab.previous)

  def nextParamTab =
    copy(paramTab = paramTab.next)

  def previousParam =
    paramTab.current match
      case Some(QueryParams(_)) => copy(query = query.previous)
      case Some(Body(bodyType)) =>
        bodyType match
          case BodyType.Raw(_)            => copy(_bodyRaw = _bodyRaw.previous)
          case BodyType.UrlEncodedForm(_) => copy(_bodyUrlEncodedForm = _bodyUrlEncodedForm.previous)
          case BodyType.MultipartForm(_)  => copy(_bodyMultipartForm = _bodyMultipartForm.previous)
          case _                          => this
      case Some(Headers(_)) => copy(header = header.previous)
      case _                => this

  def nextParam =
    paramTab.current match
      case Some(QueryParams(_)) => copy(query = query.next)
      case Some(Body(bodyType)) =>
        bodyType match
          case BodyType.Raw(_)            => copy(_bodyRaw = _bodyRaw.next)
          case BodyType.UrlEncodedForm(_) => copy(_bodyUrlEncodedForm = _bodyUrlEncodedForm.next)
          case BodyType.MultipartForm(_)  => copy(_bodyMultipartForm = _bodyMultipartForm.next)
          case _                          => this
      case Some(Headers(_)) => copy(header = header.next)
      case _                => this

  def isQueryTabSelected =
    paramTab.current match
      case Some(QueryParams(_)) => true
      case _                    => false

  def isBodyTabSelected =
    paramTab.current match
      case Some(Body(bodyType)) => true
      case _                    => false

  def isAuthTabSelected =
    paramTab.current match
      case Some(Auth(_)) => true
      case _             => false

  def isHeaderTabSelected =
    paramTab.current match
      case Some(Headers(_)) => true
      case _                => false

  def isBodyRaw =
    request.body match
      case Body(BodyType.Raw(_)) => true
      case _                     => false

  def isFirstRowSelected =
    paramTab.current match
      case Some(QueryParams(_)) => query.currentIndex == 0
      case Some(Body(bodyType)) =>
        bodyType match
          case BodyType.Raw(_)            => _bodyRaw.currentIndex == 0
          case BodyType.UrlEncodedForm(_) => _bodyUrlEncodedForm.currentIndex == 0
          case BodyType.MultipartForm(_)  => _bodyMultipartForm.currentIndex == 0
          case _                          => true
      case Some(Auth(_))    => true
      case Some(Headers(_)) => header.currentIndex == 0
      case None             => true

  def paramTabIndex = paramTab.index

  def currentTableState =
    paramTab.current.flatMap(
      _ match {
        case QueryParams(_) => Some(query.state)
        case Body(bodyType) =>
          bodyType match
            case BodyType.Raw(_)            => Some(_bodyRaw.state)
            case BodyType.UrlEncodedForm(_) => Some(_bodyUrlEncodedForm.state)
            case BodyType.MultipartForm(_)  => Some(_bodyMultipartForm.state)
            case _                          => None
        case Auth(_)    => None
        case Headers(_) => Some(header.state)
      }
    )

  def update(req: Request) =
    copy(
      request = req,
      query = query.update(
        request.queryParams.queryParams
      ),
      paramTab = paramTab.update(
        Array(
          req.queryParams,
          req.body,
          req.auth,
          req.headers
        )
      ),
      header = header.update(
        req.headers.headers
      ),
      _bodyRaw = bodyRaw(req).getOrElse(this._bodyRaw),
      _bodyUrlEncodedForm = bodyUrlEncodedForm(req).getOrElse(this._bodyUrlEncodedForm),
      _bodyMultipartForm = bodyMultiPartForm(req).getOrElse(this._bodyMultipartForm)
    )

object RequestView:
  def apply(request: Request): RequestView =
    RequestView(
      request = request,
      paramTab = Tabs(Array(request.queryParams, request.body, request.auth, request.headers)),
      header = StatefulTable(items = request.headers.headers),
      query = StatefulTable(items = request.queryParams.queryParams),
      _bodyRaw = request.body match
        case Body(BodyType.Raw(body)) => StatefulTable(items = body.split("\n"))
        case _                        => StatefulTable(items = Array())
      ,
      _bodyUrlEncodedForm = request.body match
        case Body(BodyType.UrlEncodedForm(form)) => StatefulTable(items = form.fields)
        case _                                   => StatefulTable(items = Array())
      ,
      _bodyMultipartForm = request.body match
        case Body(BodyType.MultipartForm(form)) => StatefulTable(items = form.fields)
        case _                                  => StatefulTable(items = Array())
    )
