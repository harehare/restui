package restui.models

case class Tabs[T](tabs: Array[T], index: Int = 0):
  def isEmpty = tabs.isEmpty
  def current = tabs.lift(index)

  def first =
    copy(index = 0)

  def last =
    copy(index = tabs.length - 1)

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
    copy(
      tabs = tabs,
      index = if (index > tabs.length - 1) tabs.length - 1 else index
    )
