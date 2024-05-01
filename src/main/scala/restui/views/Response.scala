package restui.views

import restui.{models, views}
import tui._
import tui.widgets.{BlockWidget, TableWidget}

case object Response:
  def render(response: models.Response, focused: Boolean): TableWidget =
    response match
      case body: models.ResponseBody =>
        val rows = body.toString.split("\n").map { row =>
          TableWidget.Row(
            Array(
              TableWidget.Cell(Text.nostyle(row))
            )
          )
        }

        TableWidget(
          block = Some(
            BlockWidget(
              borders = Borders.ALL
            )
          ),
          rows = rows,
          widths = Array(
            Constraint.Percentage(50),
            Constraint.Length(30),
            Constraint.Min(10)
          ),
          highlightStyle = if (focused) { Styles.highlightStyle }
          else { Styles.cursorStyle }
        )

      case models.ResponseHeaders(headers) =>
        val header = TableWidget.Row(
          cells = Array(
            "Key",
            "Value"
          )
            .map(h =>
              TableWidget
                .Cell(
                  Text.nostyle(h),
                  style = Style(
                    fg = Some(Colors.primaryColor),
                    addModifier = Modifier.BOLD
                  )
                )
            ),
          style = Style.DEFAULT
        )

        val rows = headers.headers.map { header =>
          TableWidget.Row(
            Array(
              TableWidget.Cell(Text.nostyle(header.key.toString)),
              TableWidget.Cell(Text.nostyle(header.value.toString))
            )
          )
        }

        TableWidget(
          block = Some(
            BlockWidget(
              borders = Borders.ALL
            )
          ),
          header = Some(header),
          rows = rows,
          widths = Array(
            Constraint.Percentage(50),
            Constraint.Length(30),
            Constraint.Min(10)
          ),
          highlightStyle = if (focused) { views.Styles.highlightStyle }
          else { views.Styles.cursorStyle }
        )
