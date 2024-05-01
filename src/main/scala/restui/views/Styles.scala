package restui.views

import tui.{Color, Modifier, Style}

case object Styles:
  def cursorStyle =
    Style(bg = Some(Colors.cursorColor), fg = Some(Color.White), addModifier = Modifier.BOLD)

  def highlightStyle =
    Style(bg = Some(Colors.highlightColor), fg = Some(Color.White), addModifier = Modifier.BOLD)

case object Colors:
  def primaryColor =
    Color.Yellow

  def errorColor =
    Color.Red

  def warnColor =
    Color.LightYellow

  def highlightColor =
    Color.Yellow

  def cursorColor =
    Color.DarkGray
