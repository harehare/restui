package restui.views

import tui.widgets.TableWidget

case class StatefulTable[T](state: TableWidget.State = TableWidget.State(), items: Array[T]):
  def current = state.selected.flatMap(items.lift(_))

  def currentIndex =
    state.selected.getOrElse(0)

  def next =
    val i = state.selected match {
      case Some(i) => if (i >= items.length - 1) 0 else i + 1
      case None    => 0
    }
    state.select(Some(i))
    this

  def previous =
    val i = state.selected match {
      case Some(i) =>
        if (i == 0) {
          items.length - 1
        } else {
          i - 1
        }
      case None => 0
    }
    state.select(Some(i))
    this

  def unselect =
    state.select(None)
    this

  def select =
    val i = state.selected match {
      case Some(i) => i
      case None    => 0
    }
    state.select(Some(i))
    this

  def update(items: Array[T]) =
    copy(items = items)
