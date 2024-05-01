package restui.views

import restui.models
import tui._
import tui.widgets.{BlockWidget, ParagraphWidget}

case object HttpDuration:
  def render(response: models.ResponseBody) = ParagraphWidget(
    text = Text.fromSpans(
      Spans.from(
        Span.styled(
          response.toDurationString,
          Style.DEFAULT.addModifier(Modifier.BOLD)
        )
      )
    ),
    style = Style.DEFAULT,
    block = Some(BlockWidget(borders = Borders.EMPTY)),
    alignment = Alignment.Right
  )
