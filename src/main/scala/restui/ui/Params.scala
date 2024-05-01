package restui.ui

import restui.{models, views}
import tui.widgets.{ParagraphWidget, TableWidget}
import tui.{Constraint, Direction, Frame, Layout, Rect, Spans}

case object Params:
  def render(f: Frame, rect: Rect, currentLocation: models.Location, requestView: models.RequestView) =
    val chunks = Layout(
      direction = Direction.Vertical,
      constraints = Array(Constraint.Length(3), Constraint.Percentage(100))
    ).split(rect)

    f.renderWidget(
      views.Tab.render(
        Array(
          models.QueryParams.toDisplayString,
          models.Body.toDisplayString,
          models.Auth.toDisplayString,
          models.Headers.toDisplayString
        )
          .map(t => Spans.nostyle(t)),
        requestView.paramTabIndex,
        currentLocation == models.Location.Param
      ),
      chunks(0)
    )

    views.Params.render(requestView.currentParams, currentLocation.isParam) match
      case p: ParagraphWidget =>
        f.renderWidget(
          p,
          chunks(1)
        )

      case t: TableWidget =>
        requestView.currentTableState.map(
          f.renderStatefulWidget(
            t,
            chunks(1)
          )(_)
        )
