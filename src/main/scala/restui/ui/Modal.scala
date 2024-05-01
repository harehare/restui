package restui.ui

import restui.models
import tui._
import tui.widgets.{BlockWidget, ClearWidget, ParagraphWidget}

def centeredRect(percentX: Int, percentY: Int, r: Rect): Rect = {
  val popupLayout = Layout(
    direction = Direction.Vertical,
    constraints = Array(
      Constraint.Percentage((100 - percentY) / 2),
      Constraint.Percentage(percentY),
      Constraint.Percentage((100 - percentY) / 2)
    )
  )
    .split(r)

  Layout(
    direction = Direction.Horizontal,
    constraints = Array(
      Constraint.Percentage((100 - percentX) / 2),
      Constraint.Percentage(percentX),
      Constraint.Percentage((100 - percentX) / 2)
    )
  )
    .split(popupLayout(1))(1)
}

def setCursor(f: Frame, rect: Rect, input: String) =
  f.setCursor(
    x = rect.x + Grapheme(input).width + 1,
    y = rect.y + 1
  )

case object EditTextModal:
  def render(f: Frame, rect: Rect, editText: models.EditTextModal) =
    val modalArea = centeredRect(60, 40, rect)
    val layout = Layout(
      direction = Direction.Vertical,
      constraints = Array(
        Constraint.Length(3),
        Constraint.Percentage(20)
      )
    )
      .split(modalArea)

    f.renderWidget(ClearWidget, f.size);

    editText.valid match
      case Left(value) =>
        val input = ParagraphWidget(
          text = Text.nostyle(editText.text),
          block = Some(
            BlockWidget(
              borders = Borders.ALL,
              title = Some(Spans.nostyle(editText.title))
            )
          )
        )
        f.renderWidget(input, layout(0))

        if (editText.isModified) {
          val error = ParagraphWidget(
            text = Text.nostyle(value),
            style = Style.DEFAULT.fg(Color.Red)
          )
          f.renderWidget(error, layout(1))
        }

        setCursor(f, layout(0), editText.text)

      case Right(value) =>
        val input = ParagraphWidget(
          text =
            if (editText.isModified && editText.text.isEmpty)
              Text.from(
                Span.styled(
                  s"Please enter ${editText.title}",
                  Style.DEFAULT.fg(Color.DarkGray)
                )
              )
            else Text.nostyle(editText.text),
          block = Some(
            BlockWidget(
              borders = Borders.ALL,
              title = Some(Spans.nostyle(editText.title))
            )
          )
        )
        f.renderWidget(input, layout(0))
        setCursor(f, layout(0), editText.text)

case object EditKeyValueModal:
  def render(f: Frame, rect: Rect, editValueModal: models.EditValueModal) =
    val modalArea = centeredRect(60, 50, rect)
    val layout = Layout(
      direction = Direction.Vertical,
      constraints = Array(
        Constraint.Length(3),
        Constraint.Length(3),
        Constraint.Length(1)
      )
    )
      .split(modalArea)

    f.renderWidget(ClearWidget, f.size);

    val nameInput = (name: String) =>
      f.renderWidget(
        ParagraphWidget(
          text = Text.nostyle(name),
          block = Some(
            BlockWidget(
              borders = Borders.ALL,
              title = Some(Spans.nostyle(editValueModal.nameTitle))
            )
          ),
          style =
            if (editValueModal.isEditName) Style.DEFAULT.fg(Color.Yellow)
            else Style.DEFAULT.fg(Color.Gray)
        ),
        layout(0)
      )
      if (editValueModal.isEditName)
        setCursor(f, layout(0), name)

    val valueInput = (value: String, error: Option[String]) =>
      f.renderWidget(
        ParagraphWidget(
          text = Text.nostyle(value),
          block = Some(
            BlockWidget(
              borders = Borders.ALL,
              title = Some(Spans.nostyle(editValueModal.valueTitle))
            )
          ),
          style =
            if (editValueModal.isEditValue) Style.DEFAULT.fg(Color.Yellow)
            else Style.DEFAULT.fg(Color.Gray)
        ),
        layout(1)
      )
      if (editValueModal.isModified) {
        error.foreach(e =>
          f.renderWidget(
            ParagraphWidget(
              text = Text.nostyle(e),
              style = Style.DEFAULT.fg(Color.Red)
            ),
            layout(2)
          )
        )
      }
      if (editValueModal.isEditValue)
        setCursor(f, layout(1), value)

    editValueModal.valid match
      case Left(errors) =>
        nameInput(editValueModal.editValue.name)
        valueInput(
          editValueModal.editValue.value,
          Some(
            errors.reduceMapLeft(identity)(_ + ", " + _)
          )
        )

      case Right(_) =>
        nameInput(editValueModal.editValue.name)
        valueInput(editValueModal.editValue.value, None)
