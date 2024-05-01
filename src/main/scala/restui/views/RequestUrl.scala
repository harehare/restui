package restui.views

import io.github.iltotore.iron.:|
import restui.{models, views}
import tui.widgets.ParagraphWidget
import tui.{Alignment, Span, Spans, Style, Text}

case object RequestUrl:
  def render(currentLocation: models.Location, method: models.Method, url: String :| models.RequestUrl): ParagraphWidget =
    ParagraphWidget(
      text = Text.fromSpans(
        Spans.from(
          Span.nostyle(" "),
          Method.render(method, currentLocation),
          Span.nostyle(" "),
          if (url.isEmpty())
            Span.styled(
              "Enter URL",
              if (currentLocation.isRequestUrl) {
                views.Styles.highlightStyle
              } else { Style.DEFAULT }
            )
          else
            Span.styled(
              url.toString,
              if (currentLocation.isRequestUrl) {
                views.Styles.highlightStyle
              } else { Style.DEFAULT }
            )
        )
      ),
      block = None,
      alignment = Alignment.Left
    )
