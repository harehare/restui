package restui.models

import restui.models

type ValidateText = (text: String) => Either[String, String]

case class EditTextModal(
    val title: String = "",
    val text: String = "",
    val location: models.Location = models.Location.Collection,
    val state: EditState = EditState.Add,
    val validate: ValidateText = (text: String) => Right(text),
    val isOpen: Boolean = false,
    val isModified: Boolean = false
):

  def input(text: String) =
    copy(text = text, isModified = true)

  def isValid = valid.isRight

  def delete =
    if (text.length > 0)
      copy(text = text.substring(0, text.length - 1))
    else
      this

  def close = copy(title = "", text = "", isOpen = false)

  def open(
      title: String,
      text: String,
      location: models.Location,
      state: EditState,
      validate: ValidateText
  ) = copy(
    title = title,
    text = text,
    location = location,
    state = state,
    validate = validate,
    isOpen = true,
    isModified = false
  )

  def valid = validate(text)

object EditTextModal:
  def init = EditTextModal()
  def initWithValidate(validate: (text: String) => Either[String, String]) =
    EditTextModal(validate = validate)
