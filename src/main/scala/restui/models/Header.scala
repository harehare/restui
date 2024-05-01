package restui.models

import io.github.iltotore.iron.constraint.all._
import io.github.iltotore.iron.zioJson.given
import io.github.iltotore.iron.{:|, refine, refineOption}
import zio.json._

type HeaderKey = (Not[Empty] & MaxLength[8192]) DescribedAs "Please enter the collection name within 8192 characters"
type HeaderValue = Not[Empty] DescribedAs "Please enter header value"

final case class Header(
    id: String :| Id,
    key: String :| HeaderKey,
    value: String :| HeaderValue
):
  override def toString = s"$key: $value"
  def toValue = zio.http.Headers(key.toString(), value.toString())
  def toCurlCommand = s"-H '$key=$value'"

  def replaceEnv(env: restui.Env) =
    for {
      replacedKey <- env.replace(key.toString).refineOption[HeaderKey]
      replacedValue <- env.replace(value.toString).refineOption[HeaderValue]
    } yield this.copy(key = replacedKey, value = replacedValue)

object Header:
  implicit val decoder: JsonDecoder[Header] = DeriveJsonDecoder.gen
  implicit val encoder: JsonEncoder[Header] = DeriveJsonEncoder.gen[Header]

  def apply(s: String): Option[Header] =
    s.split(":").map(_.trim()) match
      case Array(key, value) =>
        for {
          headerId <- Some(java.util.UUID.randomUUID.toString.refine[Id])
          headerKey <- key.refineOption[HeaderKey]
          headerValue <- value.refineOption[HeaderValue]
        } yield Header(headerId, headerKey, headerValue)
      case _ => None

  def apply(
      id: String = java.util.UUID.randomUUID.toString,
      name: String,
      value: String
  ): Option[Header] =
    for {
      headerId <- id.refineOption[Id]
      headerKey <- name.refineOption[HeaderKey]
      headerValue <- value.refineOption[HeaderValue]
    } yield Header(headerId, headerKey, headerValue)
