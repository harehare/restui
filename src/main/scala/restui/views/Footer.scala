package restui.views

import restui.{models, views}
import tui._
import tui.widgets.ParagraphWidget

case object Footer:
  def render(currentLocation: models.Location, isModalVisible: Boolean, message: Option[models.Message]) =
    message match
      case Some(models.Message.Info(m)) =>
        ParagraphWidget(
          text = Text.fromSpans(
            Spans.from(
              Span.styled(
                s" $m",
                Style.DEFAULT.fg(views.Colors.primaryColor).addModifier(Modifier.BOLD)
              )
            )
          ),
          style = Style.DEFAULT,
          block = None,
          alignment = Alignment.Left
        )
      case Some(models.Message.Warn(m)) =>
        ParagraphWidget(
          text = Text.fromSpans(
            Spans.from(
              Span.styled(
                s" $m",
                Style.DEFAULT.fg(views.Colors.warnColor).addModifier(Modifier.BOLD)
              )
            )
          ),
          style = Style.DEFAULT,
          block = None,
          alignment = Alignment.Left
        )
      case Some(models.Message.Error(m)) =>
        ParagraphWidget(
          text = Text.fromSpans(
            Spans.from(
              Span.styled(
                s" $m",
                Style.DEFAULT.fg(views.Colors.errorColor).addModifier(Modifier.BOLD)
              )
            )
          ),
          style = Style.DEFAULT,
          block = None,
          alignment = Alignment.Left
        )
      case _ if isModalVisible =>
        ParagraphWidget(
          text = Text.fromSpans(
            Spans.from(
              (
                renderTips("↑↓", "Move") ++
                  renderTips("esc", "Close")
              )*
            )
          ),
          style = Style.DEFAULT,
          block = None,
          alignment = Alignment.Left
        )
      case _ =>
        ParagraphWidget(
          text = Text.fromSpans(Spans.from((currentLocation match
            case models.Location.Collection =>
              renderCommonTips ++
                renderTips("a", "Add") ++
                renderTips("e", "Edit") ++
                renderTips("Ctrl + d", "Delete")
            case models.Location.Request =>
              renderCommonTips ++
                renderTips("a", "Add") ++
                renderTips("c", "Copy curl command") ++
                renderTips("d", "Duplicate") ++
                renderTips("e", "Edit") ++
                renderTips("Ctrl + d", "Delete")
            case models.Location.Method =>
              renderCommonTips ++ renderTips("c", "Copy curl command") ++ renderTips("m", "Change method")
            case models.Location.RequestUrl =>
              renderCommonTips ++ renderTips("c", "Copy curl command") ++ renderTips("e", "Edit URL")
            case models.Location.Param =>
              renderCommonTips ++
                renderTips("a", "Add") ++
                renderTips("c", "Copy curl command") ++
                renderTips("e", "Edit") ++
                renderTips("Ctrl + d", "Delete") ++
                renderTips("m", "Change body type")
            case models.Location.Response =>
              renderCommonTips ++ renderTips("c", "Copy selected rows")
          )*)),
          style = Style.DEFAULT,
          block = None,
          alignment = Alignment.Left
        )

  private def renderCommonTips =
    Span.styled(
      "Tips: ",
      Style(addModifier = Modifier.BOLD)
    ) :: renderTips("←↑↓→", "Move cursor") ++ renderTips("Ctrl + r", "Send request") ++ renderTips(
      "q",
      "Quit"
    ) ++ renderTips(
      "s",
      "Save"
    )

  private def renderTips(key: String, description: String) =
    List(
      Span.styled(
        "<",
        Style.DEFAULT.fg(Color.DarkGray)
      ),
      Span.styled(
        key,
        Style.DEFAULT.fg(Color.LightRed)
      ),
      Span.styled(
        ">",
        Style.DEFAULT.fg(Color.DarkGray)
      ),
      Span.styled(
        " => ",
        Style.DEFAULT
      ),
      Span.styled(
        s"${description} ",
        Style.DEFAULT.fg(views.Colors.primaryColor)
      )
    )
