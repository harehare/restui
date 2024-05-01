package restui.views

import restui.models
import tui.{Color, Modifier, Span, Style}

case object Method:
  def render(method: models.Method, currentLocation: models.Location = models.Location.Request) = Span.styled(
    method.toString,
    if (currentLocation.isMethod)
      Styles.highlightStyle
    else
      Style(fg = Some(method match
        case models.Method.Get =>
          Color.LightGreen
        case models.Method.Post =>
          Color.LightMagenta
        case models.Method.Put =>
          Color.LightYellow
        case models.Method.Patch =>
          Color.LightBlue
        case models.Method.Delete =>
          Color.LightRed
        case models.Method.Head =>
          Color.Gray
      )).addModifier(Modifier.BOLD)
  )
