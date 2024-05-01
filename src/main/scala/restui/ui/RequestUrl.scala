package restui.ui

import restui.{models, views}
import tui.{Frame, Rect}

case object RequestUrl:
  def render(f: Frame, rect: Rect, currentLocation: models.Location, request: models.Request) =
    f.renderWidget(views.RequestUrl.render(currentLocation, request.method, request.url), rect)
