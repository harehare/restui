package restui.views

import scala.reflect.ClassTag

import tui.widgets.ListWidget

case class StatefulList[T](state: ListWidget.State = ListWidget.State(),items: Array[T]):
  def current: Option[T] = state.selected.flatMap(items.lift(_))
  def add(item: T)(implicit ev: ClassTag[T]) = copy(items = items :+ item)
  def update(items: Array[T])(implicit ev: ClassTag[T]) = copy(items = items)

  def updateCurrent(updateItem: T)(implicit ev: ClassTag[T]) =
    state.selected match {
      case Some(i) =>
        copy(items = items.zipWithIndex.map { case (item, index) =>
          if (i == index) updateItem else item
        })
      case None => this
    }

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

  def select: StatefulList[T] =
    val i = state.selected match {
      case Some(i) => i
      case None    => 0
    }
    state.select(Some(i))
    this

  def last =
    state.select(Some(items.length - 1))
    this
