package restui.models

import java.net.URLDecoder

import io.github.iltotore.iron.constraint.all._
import io.github.iltotore.iron.zioJson.given
import io.github.iltotore.iron.{:|, refine, refineOption}
import io.netty.util.CharsetUtil
import zio.Chunk
import zio.json._

type QueryName = Not[Empty] DescribedAs "Please enter query name"
type QueryValue = Not[Empty] DescribedAs "Please enter query value"

case class Query(
    id: String :| Id,
    key: String :| QueryName,
    value: String :| QueryValue
):
  override def toString = s"$key=$value"
  def toValue = (key.toString, Chunk(value.toString))
  def toCurlCommand = s"--data-urlencode '$key=$value'"

  def replaceEnv(env: restui.Env) =
    for {
      replacedKey <- env.replace(key.toString).refineOption[QueryName]
      replacedValue <- env.replace(value.toString).refineOption[QueryValue]
    } yield this.copy(key = replacedKey, value = replacedValue)

object Query:
  given JsonCodec[Query] = DeriveJsonCodec.gen

  def apply(q: String): Option[Query] =
    q.split("=").map(_.trim()) match
      case Array(key, value) =>
        for {
          queryId <- Some(java.util.UUID.randomUUID.toString.refine[Id])
          queryKey <- key.refineOption[QueryName]
          queryValue <- URLDecoder
            .decode(value, CharsetUtil.UTF_8.name())
            .refineOption[QueryValue]
        } yield Query(
          queryId,
          queryKey,
          queryValue
        )
      case _ => None

  def apply(
      id: String = java.util.UUID.randomUUID.toString,
      key: String,
      value: String
  ): Option[Query] =
    for
      queryId <- id.refineOption[Id]
      queryKey <- key.refineOption[QueryName]
      queryValue <- value.refineOption[QueryValue]
    yield Query(queryId, queryKey, queryValue)
