package restui.ui

import restui.{models, ui, views}
import tui._
import tui.widgets.{BlockWidget, ParagraphWidget}

case object Request:
  def render(f: Frame, app: models.App, rect: Rect): Unit =
    val chunks = Layout(
      direction = Direction.Vertical,
      constraints = Array(Constraint.Length(1), Constraint.Percentage(100))
    ).split(rect)

    val requestUrlChunk = chunks(0)
    val requestAndResponseChunks = chunks(1)

    val chunks2 = Layout(
      direction = Direction.Horizontal,
      constraints = Array(Constraint.Percentage(50), Constraint.Percentage(50))
    ).split(chunks(1))

    val requestChunk = chunks2(0)
    val responseChunk = chunks2(1)

    ui.RequestUrl.render(f, requestUrlChunk, app.currentLocation, app.currentRequest)
    ui.Params.render(
      f,
      requestChunk,
      app.currentLocation,
      app.requestView
    )

    // response
    app.responseView match
      case (Some(responseView)) =>
        ui.Response.render(
          f,
          responseChunk,
          app.currentLocation,
          responseView
        )

      case _ if app.isLoading =>
        f.renderWidget(views.Loading.render("Sending"), ui.Layout.center(responseChunk))

      case _ =>
        f.renderWidget(
          BlockWidget(
            borders = Borders.ALL
          ),
          responseChunk
        )
        f.renderWidget(
          ParagraphWidget(
            text = Text.nostyle("None"),
            alignment = Alignment.Center
          ),
          ui.Layout.center(responseChunk)
        )
