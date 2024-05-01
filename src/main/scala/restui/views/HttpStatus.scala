package restui.views

import restui.models
import tui._
import tui.widgets.{BlockWidget, ParagraphWidget}

case object HttpStatus:
  def render(status: models.ResponseStatus): ParagraphWidget = ParagraphWidget(
    text = Text.fromSpans(
      Spans.from(
        Span.styled(
          status.toString,
          Style.DEFAULT
            .fg(status.code match
              case code if code < 300 => Color.Green
              case code if code < 500 => Color.Yellow
              case _                  => Color.Red,
            )
            .addModifier(Modifier.BOLD)
        )
      )
    ),
    style = Style.DEFAULT,
    block = Some(BlockWidget(borders = Borders.EMPTY)),
    alignment = Alignment.Right
  )
