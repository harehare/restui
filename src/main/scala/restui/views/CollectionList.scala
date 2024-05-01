package restui.views

import restui.models.{Collection, Location}
import restui.{models, views}
import tui._
import tui.widgets.{BlockWidget, ListWidget}

case object CollectionList:
  def render(currentLocation: Location, collections: Array[Collection]): ListWidget =
    ListWidget(
      items = collections
        .map(c =>
          ListWidget.Item(
            Text.fromSpans(
              Spans.from(
                Span.styled(
                  c.name,
                  Style(addModifier = Modifier.BOLD)
                ),
                Span
                  .styled(
                    if (c.modified) " *" else "",
                    Style.DEFAULT.addModifier(Modifier.BOLD).fg(views.Colors.highlightColor)
                  )
              )
            )
          )
        ),
      block = Some(
        BlockWidget(
          borders = Borders.ALL,
          title = Some(Spans.nostyle("Collections"))
        )
      ),
      startCorner = Corner.TopLeft,
      highlightStyle = if (currentLocation == models.Location.Collection) {
        views.Styles.highlightStyle
      } else { views.Styles.cursorStyle },
      highlightSymbol = Some("> ")
    )
