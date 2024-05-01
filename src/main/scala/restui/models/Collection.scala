package restui.models

import io.github.iltotore.iron.constraint.all._
import io.github.iltotore.iron.zioJson.given
import io.github.iltotore.iron.{:|, refine}
import restui.models
import zio._
import zio.json._

type CollectionName = (Not[Empty] & MaxLength[32]) DescribedAs "Please enter the collection name within 32 characters"

case class Collection(
    id: String :| restui.models.Id,
    name: String :| CollectionName,
    requests: Array[Request] = Array(),
    modified: Boolean = false
):
  def changeName(name: String :| CollectionName) = copy(name = name, modified = true)
  def addRequest(request: Request) = copy(requests = requests :+ request)
  def updateRequest(request: Request) = copy(requests = requests.map(r => if (r.id == request.id) request else r))
  def deleteRequest(request: Request) = copy(requests = requests.filter(r => r.id != request.id))

object Collection:
  given JsonCodec[Collection] = DeriveJsonCodec.gen
  def init =
    Collection(
      id = java.util.UUID.randomUUID.toString.refine[restui.models.Id],
      name = "collection1".refine[CollectionName],
      requests = Array(Request.create("request1"))
    )
