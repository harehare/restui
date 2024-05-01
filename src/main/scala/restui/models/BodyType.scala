package restui.models
import io.github.iltotore.iron.constraint.all._
import zio.json._

type BodyFile = Not[Empty] DescribedAs "Please enter file"

enum BodyType:
  case NoBody
  case Raw(raw: String)
  case UrlEncodedForm(form: Form)
  case MultipartForm(form: Form)

  def toCurlCommand =
    this match
      case NoBody               => ""
      case Raw(raw)             => s"-d '$raw'"
      case UrlEncodedForm(form) => form.fields.map(field => s"-F '${field.key}=${field.value}'").mkString(" ")
      case MultipartForm(form) =>
        form.fields.map(field => s"-F '${field.key}=${if (field.isFile) s"@${field.value}" else field.value}'").mkString(" ")

  def replaceEnv(env: restui.Env) =
    this match
      case NoBody               => this
      case Raw(raw)             => Raw(env.replace(raw))
      case UrlEncodedForm(form) => UrlEncodedForm(form.replaceEnv(env))
      case MultipartForm(form)  => MultipartForm(form.replaceEnv(env))

object BodyType:
  given JsonCodec[BodyType] = DeriveJsonCodec.gen
