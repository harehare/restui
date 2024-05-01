package restui.views

import restui.{models, views}
import tui._
import tui.widgets.{BlockWidget, ParagraphWidget, TableWidget}

case object Params:
  def render(params: models.Params, focused: Boolean): TableWidget | ParagraphWidget =
    val (title, header, rows) = params match
      case models.QueryParams(queryParams) =>
        (
          None,
          Some(
            TableWidget.Row(
              cells = Array(
                "Key",
                "Value"
              )
                .map(h =>
                  TableWidget
                    .Cell(
                      Text.nostyle(h),
                      style = Style(fg = Some(Colors.primaryColor), addModifier = Modifier.BOLD)
                    )
                )
            )
          ),
          queryParams.map { param =>
            TableWidget.Row(
              Array(
                TableWidget.Cell(Text.nostyle(param.key.toString)),
                TableWidget.Cell(Text.nostyle(param.value.toString))
              )
            )
          }
        )
      case models.Body(models.BodyType.Raw(raw)) =>
        (
          Some("Raw"),
          None,
          raw.split("\n").map { row =>
            TableWidget.Row(
              Array(
                TableWidget.Cell(Text.nostyle(row))
              )
            )
          }
        )
      case models.Body(models.BodyType.UrlEncodedForm(form)) =>
        (
          Some("Form URL Encoded"),
          Some(
            TableWidget.Row(
              cells = Array(
                "Key",
                "Value"
              )
                .map(h =>
                  TableWidget
                    .Cell(
                      Text.nostyle(h),
                      style = Style(fg = Some(Colors.primaryColor), addModifier = Modifier.BOLD)
                    )
                )
            )
          ),
          form.fields.map { param =>
            TableWidget.Row(
              Array(
                TableWidget.Cell(Text.nostyle(param.key.toString)),
                TableWidget.Cell(Text.nostyle(param.value.toString))
              )
            )
          }
        )
      case models.Body(models.BodyType.MultipartForm(form)) =>
        (
          Some("Multipart Form"),
          Some(
            TableWidget.Row(
              cells = Array(
                "Key",
                "Value"
              )
                .map(h =>
                  TableWidget
                    .Cell(
                      Text.nostyle(h),
                      style = Style(fg = Some(Colors.primaryColor), addModifier = Modifier.BOLD)
                    )
                )
            )
          ),
          form.fields.map { param =>
            TableWidget.Row(
              Array(
                TableWidget.Cell(Text.nostyle(param.key.toString)),
                TableWidget.Cell(Text.nostyle(param.value.toString))
              )
            )
          }
        )
      case models.Body(models.BodyType.NoBody) =>
        return ParagraphWidget(
          text = Text.fromSpans(
            Spans.from(
              Span.styled(
                " No Body",
                Style.DEFAULT
              )
            )
          ),
          block = None,
          alignment = Alignment.Left
        )
      case models.Auth(models.AuthType.NoAuth) =>
        return ParagraphWidget(
          text = Text.fromSpans(
            Spans.from(
              Span.styled(
                " No Auth",
                Style.DEFAULT
              )
            )
          ),
          block = None,
          alignment = Alignment.Left
        )
      case models.Auth(models.AuthType.Basic(userName, password)) =>
        return ParagraphWidget(
          text = Text.fromSpans(
            Spans.from(
              Span.styled(
                models.AuthType.Basic(userName, password).toString,
                Style.DEFAULT
              )
            )
          ),
          block = Some(BlockWidget(borders = Borders.ALL, title = Some(Spans.nostyle("Basic Auth")))),
          alignment = Alignment.Left
        )
      case models.Auth(models.AuthType.BearerToken(token)) =>
        return ParagraphWidget(
          text = Text.fromSpans(
            Spans.from(
              Span.styled(
                token,
                Style.DEFAULT
              )
            )
          ),
          block = Some(
            BlockWidget(
              borders = Borders.ALL,
              title = Some(Spans.nostyle("Token"))
            )
          ),
          alignment = Alignment.Left
        )
      case models.Headers(headers) =>
        (
          None,
          Some(
            TableWidget.Row(
              cells = Array(
                "Key",
                "Value"
              )
                .map(h =>
                  TableWidget
                    .Cell(
                      Text.nostyle(h),
                      style = Style(fg = Some(Colors.primaryColor), addModifier = Modifier.BOLD)
                    )
                )
            )
          ),
          headers.map { header =>
            TableWidget.Row(
              Array(
                TableWidget.Cell(Text.nostyle(header.key.toString)),
                TableWidget.Cell(Text.nostyle(header.value.toString))
              )
            )
          }
        )

    TableWidget(
      block = Some(
        BlockWidget(
          title = title.map(Spans.nostyle(_)),
          borders = Borders.ALL
        )
      ),
      header = header,
      rows = rows,
      widths = Array(
        Constraint.Percentage(50),
        Constraint.Length(30),
        Constraint.Min(10)
      ),
      highlightStyle = if (focused) { Styles.highlightStyle }
      else { Styles.cursorStyle }
    )
