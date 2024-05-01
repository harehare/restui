package restui.models

import java.io.File
import java.nio.file.{Files, Paths}

import io.github.iltotore.iron.constraint.all._
import io.github.iltotore.iron.zioJson.given
import io.github.iltotore.iron.{:|, refineOption}
import zio.Chunk
import zio.http.MediaType
import zio.json._

type FormKey = Not[Empty] DescribedAs "Please enter form key"
type FormValue = Not[Empty] DescribedAs "Please enter form value"

case class FormField(
    id: String :| Id,
    key: String :| FormKey,
    value: String :| FormValue
):
  override def toString = s"$key=$value"
  def isFile = Files.exists(Paths.get(value))

  def replaceEnv(env: restui.Env) =
    for {
      replacedKey <- env.replace(key.toString).refineOption[FormKey]
      replacedValue <- env.replace(value.toString).refineOption[FormValue]
    } yield this.copy(key = replacedKey, value = replacedValue)

object FormField:
  given JsonCodec[FormField] = DeriveJsonCodec.gen

  def apply(
      id: String = java.util.UUID.randomUUID.toString,
      key: String,
      value: String
  ): Option[FormField] =
    for
      formId <- id.refineOption[Id]
      formKey <- key.refineOption[FormKey]
      formValue <- value.refineOption[FormValue]
    yield FormField(formId, formKey, formValue)

case class Form(fields: Array[FormField], isMultipartForm: Boolean = false):
  def toValue =
    zio.http.Form(
      fields.flatMap(formField =>
        if (formField.isFile && isMultipartForm)
          val file = new File(formField.value)
          val ext = file.getName.split("\\.").lastOption.getOrElse("")
          MediaType
            .forFileExtension(ext)
            .map(
              zio.http.FormField
                .binaryField(formField.key, Chunk.fromArray(Files.readAllBytes(Paths.get(formField.value.toString))), _)
            )
        else Some(zio.http.FormField.simpleField(formField.key, formField.value))
      )*
    )

  override def toString =
    fields.foldLeft("")(_ + "&" + _)

  def add(formField: FormField) =
    copy(fields = fields :+ formField)

  def replaceEnv(env: restui.Env) =
    copy(fields = fields.map(v => v.replaceEnv(env).getOrElse(v)))

object Form:
  given JsonCodec[Form] = DeriveJsonCodec.gen
