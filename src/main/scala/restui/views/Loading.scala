package restui.views

import tui.widgets.ParagraphWidget
import tui.{Alignment, Modifier, Span, Spans, Style, Text}

case object Loading:
  def render(text: String): ParagraphWidget =
    ParagraphWidget(
      text = Text.fromSpans(
        Spans.from(
          Span
            .styled(s"${text}...", Style.DEFAULT.addModifier(Modifier.BOLD))
        )
      ),
      style = Style.DEFAULT,
      block = None,
      alignment = Alignment.Center
    )
