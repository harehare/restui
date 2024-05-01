package restui.views

case class StatefulTab[T](tabs: Array[T], index: Int = 0):
  def current: Option[T] = tabs.lift(index)
  def isEmpty = tabs.isEmpty

  def first =
    copy(index = 0)

  def next =
    if (index + 1 > tabs.length - 1)
      copy(index = 0)
    else
      copy(index = index + 1)

  def previous =
    if (index > 0)
      copy(index = index - 1)
    else
      copy(index = tabs.length - 1)

  def update(tabs: Array[T]) =
    copy(tabs = tabs, index = 0)
