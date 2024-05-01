package restui.ui

import restui.{models, views}
import tui.{Frame, Rect}

case object Footer:
  def render(f: Frame, rect: Rect, currentLocation: models.Location, isModalVisible: Boolean, message: Option[models.Message]) =
    f.renderWidget(views.Footer.render(currentLocation, isModalVisible, message), rect)
