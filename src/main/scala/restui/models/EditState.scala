package restui.models

enum EditState:
  case Add
  case Edit

  def isAdd = this == Add
  def isEdit = this == Edit
