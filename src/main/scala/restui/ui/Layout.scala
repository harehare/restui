package restui.ui
import tui.{Constraint, Direction, Margin, Rect}

object Layout:
  def center(rect: Rect) =
    val chunks = tui
      .Layout(
        direction = Direction.Vertical,
        constraints = Array(Constraint.Percentage(50), Constraint.Percentage(50)),
        margin = Margin(1)
      )
      .split(rect)
    chunks(1)
