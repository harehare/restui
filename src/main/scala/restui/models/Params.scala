package restui.models
import io.github.iltotore.iron.constraint.all._
import io.netty.util.CharsetUtil
import zio.Chunk
import zio.http.{Boundary, QueryParams}
import zio.json._

sealed abstract class Params

final case class Body(value: BodyType) extends Params:
  def toCurlCommand = value.toCurlCommand
  def toValue =
    value match
      case BodyType.NoBody =>
        zio.http.Body.empty
      case BodyType.Raw(v) =>
        zio.http.Body.fromString(v.toString(), CharsetUtil.UTF_8)
      case BodyType.UrlEncodedForm(form) =>
        zio.http.Body
          .fromURLEncodedForm(form.toValue, CharsetUtil.UTF_8)
      case BodyType.MultipartForm(form) =>
        Boundary
          .fromString(form.toString, CharsetUtil.UTF_8)
          .map(
            zio.http.Body
              .fromMultipartForm(form.toValue, _)
          )
          .getOrElse(zio.http.Body.empty)

  override def toString =
    value match
      case BodyType.NoBody =>
        ""
      case BodyType.Raw(v) =>
        v
      case BodyType.UrlEncodedForm(queryParams) =>
        ""
      case BodyType.MultipartForm(queryParams) =>
        ""

  def replaceEnv(env: restui.Env) =
    copy(value = value.replaceEnv(env))

object Body:
  implicit val decoder: JsonDecoder[Body] = DeriveJsonDecoder.gen
  implicit val encoder: JsonEncoder[Body] = DeriveJsonEncoder.gen[Body]
  def toDisplayString = "Body"
  def noBody = Body(value = BodyType.NoBody)

final case class Headers(headers: Array[Header]) extends Params:
  def toValue = headers.foldLeft(zio.http.Headers.empty)(_ ++ _.toValue)

  def add(header: Header) =
    copy(headers = headers :+ header)

  def replaceEnv(env: restui.Env) =
    copy(headers = headers.map(v => v.replaceEnv(env).getOrElse(v)))

object Headers:
  implicit val decoder: JsonDecoder[Headers] = DeriveJsonDecoder.gen
  implicit val encoder: JsonEncoder[Headers] = DeriveJsonEncoder.gen[Headers]
  def toDisplayString = "Headers"

case class QueryParams(queryParams: Array[Query]) extends Params:
  def toValue =
    zio.http.QueryParams(
      queryParams.foldLeft(Map.empty[String, Chunk[String]])(_ + _.toValue)
    )

  override def toString =
    queryParams.foldLeft("")(_ + "&" + _)

  def add(query: Query) =
    copy(queryParams = queryParams :+ query)

  def replaceEnv(env: restui.Env) =
    copy(queryParams = queryParams.map(v => v.replaceEnv(env).getOrElse(v)))

object QueryParams:
  implicit val decoder: JsonDecoder[QueryParams] = DeriveJsonDecoder.gen
  implicit val encoder: JsonEncoder[QueryParams] =
    DeriveJsonEncoder.gen[QueryParams]
  def toDisplayString = "Params"

final case class Auth(auth: AuthType) extends Params:
  def toCurlCommand = auth.toCurlCommand

  def replaceEnv(env: restui.Env) =
    copy(auth = auth.replaceEnv(env))

object Auth:
  implicit val decoder: JsonDecoder[Auth] = DeriveJsonDecoder.gen
  implicit val encoder: JsonEncoder[Auth] = DeriveJsonEncoder.gen[Auth]
  def noAuth = Auth(AuthType.NoAuth)
  def toDisplayString = "Auth"
