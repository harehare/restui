package restui.models
import zio.prelude.Validation

enum CurrentEdit:
  case Name
  case Value

case class EditValue(
    name: String = "",
    value: String = ""
)

type ValidateEditValue = String => Either[String, String]

case class EditValueModal(
    val nameTitle: String = "",
    val valueTitle: String = "",
    val editValue: EditValue = EditValue(),
    val location: Location = Location.Collection,
    val state: EditState = EditState.Add,
    val isOpen: Boolean = false,
    val validateName: ValidateEditValue = _ => Right(""),
    val validateValue: ValidateEditValue = _ => Right(""),
    val isModified: Boolean = false,
    private val currentEdit: CurrentEdit = CurrentEdit.Name
):
  def close =
    copy(editValue = EditValue(), isOpen = false, isModified = false, currentEdit = CurrentEdit.Name)

  def currentText =
    currentEdit match
      case CurrentEdit.Name =>
        editValue.name
      case CurrentEdit.Value =>
        editValue.value

  def delete =
    currentEdit match
      case CurrentEdit.Name if editValue.name.length > 0 =>
        copy(editValue = editValue.copy(name = editValue.name.substring(0, editValue.name.length - 1)))
      case CurrentEdit.Value if editValue.value.length > 0 =>
        copy(editValue = editValue.copy(value = editValue.value.substring(0, editValue.value.length - 1)))
      case _ => this

  def move =
    currentEdit match
      case CurrentEdit.Name =>
        copy(currentEdit = CurrentEdit.Value)
      case CurrentEdit.Value =>
        copy(currentEdit = CurrentEdit.Name)

  def isEditName = currentEdit == CurrentEdit.Name

  def isEditValue = currentEdit == CurrentEdit.Value

  def isValid = valid.isRight

  def input(text: String) =
    currentEdit match
      case CurrentEdit.Name =>
        copy(editValue = editValue.copy(name = text), isModified = true)
      case CurrentEdit.Value =>
        copy(editValue = editValue.copy(value = text), isModified = true)

  def open(
      nameTitle: String,
      valueTitle: String,
      editValue: EditValue = EditValue(),
      location: Location = Location.Collection,
      state: EditState,
      validateName: ValidateEditValue,
      validateValue: ValidateEditValue
  ) = copy(
    nameTitle = nameTitle,
    valueTitle = valueTitle,
    editValue = editValue,
    location = location,
    state = state,
    isOpen = true,
    validateName = validateName,
    validateValue = validateValue,
    isModified = false
  )

  def valid = Validation
    .validateWith(
      Validation.fromEither(validateName(editValue.name)),
      Validation.fromEither(validateValue(editValue.value))
    )(EditValue.apply)
    .toEither

object EditValueModal:
  def init = EditValueModal()
  def initWithValidate(
      validateName: ValidateEditValue,
      validateValue: ValidateEditValue
  ) =
    EditValueModal(
      validateName = validateName,
      validateValue = validateValue
    )
