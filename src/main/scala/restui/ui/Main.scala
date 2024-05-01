package restui.ui

import restui.{models, ui, views}
import tui._
import tui.widgets.{BlockWidget, ParagraphWidget}

case object Main:
  def render(f: Frame, app: models.App) =
    val mainChunks = Layout(
      direction = Direction.Vertical,
      constraints = Array(Constraint.Length(f.size.height - 1), Constraint.Length(1))
    ).split(f.size)

    val chunks = Layout(
      direction = Direction.Horizontal,
      constraints = Array(Constraint.Min(32), Constraint.Percentage(100))
    ).split(mainChunks(0))

    val chunks2 = Layout(
      direction = Direction.Vertical,
      constraints = Array(Constraint.Length(3), Constraint.Percentage(100))
    ).split(chunks(1))

    val requestTabChunk = chunks2(0)
    val requestChunk = chunks2(1)

    ui.Request.render(f, app, requestChunk)

    val collectionList =
      views.CollectionList.render(app.currentLocation, app.collectionList.items)
    val requestTabs = views.Tab.render(
      app.collectionList.current
        .map(currentCollection =>
          currentCollection.requests.map(request =>
            Spans.from(
              views.Method.render(request.method),
              Span.nostyle(" "),
              Span.nostyle(request.name.toString),
              Span
                .styled(
                  if (request.modified) " *" else "",
                  Style.DEFAULT.addModifier(Modifier.BOLD).fg(views.Colors.highlightColor)
                )
            )
          )
        )
        .getOrElse(Array()),
      app.requestTab.index,
      app.currentLocation == models.Location.Request
    )

    f.renderStatefulWidget(
      collectionList,
      chunks(0)
    )(app.collectionList.state)

    f.renderWidget(
      if (app.requestTab.isEmpty)
        ParagraphWidget(
          text = Text.nostyle("None"),
          block = Some(BlockWidget(borders = Borders.ALL)),
          style = Style.DEFAULT.fg(Color.Yellow)
        )
      else requestTabs,
      requestTabChunk
    )

    if (app.editTextModal.isOpen)
      EditTextModal.render(f, f.size, app.editTextModal)
    else if (app.editValueModal.isOpen)
      EditKeyValueModal.render(f, f.size, app.editValueModal)

    ui.Footer.render(f, mainChunks(1), app.currentLocation, app.editTextModal.isOpen || app.editValueModal.isOpen, app.message)
