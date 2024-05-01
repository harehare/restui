package restui.views
import tui.widgets.BlockWidget
import tui.widgets.tabs.TabsWidget
import tui.{Borders, Span, Spans, Style, symbols}

case object Tab:
  def render(titles: Array[Spans], index: Int, selected: Boolean): TabsWidget =
    TabsWidget(
      titles = titles,
      block = Some(
        BlockWidget(
          borders = Borders.ALL,
          borderStyle = Style.DEFAULT
        )
      ),
      selected = index,
      highlightStyle = if (selected) { Styles.highlightStyle }
      else { Styles.cursorStyle },
      divider = Span.nostyle(symbols.line.THICK_VERTICAL)
    )
