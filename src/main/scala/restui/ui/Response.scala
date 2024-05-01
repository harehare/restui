package restui.ui

import restui.{models, views}
import tui.{Constraint, Direction, Frame, Layout, Rect, Spans}

case object Response:
  def render(f: Frame, rect: Rect, currentLocation: models.Location, responseView: models.ResponseView) =
    val chunks = Layout(
      direction = Direction.Vertical,
      constraints = Array(Constraint.Length(3), Constraint.Percentage(100))
    ).split(rect)

    responseView.current match
      case Some(models.ResponseBody(status, body, duration, bodyType)) =>
        val headerChunks = Layout(
          direction = Direction.Horizontal,
          constraints = Array(Constraint.Length(rect.width - 28), Constraint.Max(20), Constraint.Length(8))
        ).split(chunks(0))

        f.renderWidget(
          views.Tab.render(
            Array(
              models.ResponseBody.toDisplayString,
              models.ResponseHeaders.toDisplayString
            )
              .map(t => Spans.nostyle(t)),
            responseView.tab.index,
            currentLocation == models.Location.Response
          ),
          headerChunks(0)
        )

        f.renderWidget(
          views.HttpStatus.render(status),
          headerChunks(1)
        )
        f.renderWidget(
          views.HttpDuration.render(models.ResponseBody(status, body, duration, bodyType)),
          headerChunks(2)
        )
        f.renderStatefulWidget(
          views.Response
            .render(models.ResponseBody(status, body, duration, bodyType), currentLocation == models.Location.Response),
          chunks(1)
        )(responseView.body.state)

      case Some(models.ResponseHeaders(headers)) =>
        f.renderWidget(
          views.Tab.render(
            Array(
              models.ResponseBody.toDisplayString,
              models.ResponseHeaders.toDisplayString
            )
              .map(t => Spans.nostyle(t)),
            responseView.tab.index,
            currentLocation == models.Location.Response
          ),
          chunks(0)
        )

        f.renderStatefulWidget(
          views.Response.render(models.ResponseHeaders(headers), currentLocation == models.Location.Response),
          chunks(1)
        )(responseView.header.state)

      case None => ()
