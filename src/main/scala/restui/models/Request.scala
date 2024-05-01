package restui.models

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.{Duration, MILLISECONDS}

import io.github.iltotore.iron.constraint.all._
import io.github.iltotore.iron.zioJson.given
import io.github.iltotore.iron.{:|, refine, refineOption}
import zio._
import zio.http.{Client, URL}
import zio.json._

type RequestName = Not[Empty] DescribedAs "Please enter request name"
type RequestUrl = (Not[Empty] & (ValidURL | Env)) DescribedAs "Please enter in URL format"

enum Method:
  case Get
  case Post
  case Put
  case Patch
  case Delete
  case Head

  override def toString(): String = this match
    case Get    => "GET"
    case Post   => "POST"
    case Put    => "PUT"
    case Patch  => "PATCH"
    case Delete => "DELETE"
    case Head   => "HEAD"

object Method:
  given JsonCodec[Method] = DeriveJsonCodec.gen

case class Request(
    id: String :| Id,
    name: String :| RequestName,
    headers: Headers,
    queryParams: QueryParams,
    body: Body,
    url: String :| RequestUrl,
    method: Method,
    auth: Auth,
    modified: Boolean = false
):
  def addHeader(header: Header) = copy(headers = headers.add(header), modified = true)
  def addQuery(query: Query) = copy(queryParams = queryParams.add(query), modified = true)
  def addFormField(field: FormField) =
    body match
      case Body(BodyType.UrlEncodedForm(form)) => copy(body = Body(BodyType.UrlEncodedForm(form.add(field))), modified = true)
      case Body(BodyType.MultipartForm(form))  => copy(body = Body(BodyType.MultipartForm(form.add(field))), modified = true)
      case _                                   => this

  def changeName(name: String :| RequestName) = copy(name = name, modified = true)
  def changeMethod(method: Method) = copy(method = method, modified = true)
  def changeUrl(url: String :| RequestUrl) = copy(url = url, modified = true)
  def changeBody(body: Body) = copy(body = body, modified = true)
  def changeAuth(auth: Auth) = copy(auth = auth, modified = true)
  def deleteHeader(header: Header) = copy(headers = Headers(headers.headers.filter(_.id != header.id)), modified = true)
  def deleteQuery(query: Query) =
    copy(queryParams = QueryParams(queryParams.queryParams.filter(_.id != query.id)), modified = true)
  def deleteFormField(field: FormField) = copy(
    body = body match
      case Body(BodyType.UrlEncodedForm(form)) => Body(BodyType.UrlEncodedForm(Form(form.fields.filter(_.id != field.id))))
      case Body(BodyType.MultipartForm(form))  => Body(BodyType.MultipartForm(Form(form.fields.filter(_.id != field.id))))
      case _                                   => body
    ,
    modified = true
  )

  def duplicate =
    copy(
      id = java.util.UUID.randomUUID.toString.refine[Id],
      name = s"Copy of ${name.toString}".refine,
      modified = true
    )

  def updateHeader(header: Header) =
    copy(
      headers = Headers(
        headers.headers.map(h => if (h.id == header.id) header else h)
      ),
      modified = true
    )

  def updateQuery(query: Query) =
    copy(
      queryParams = QueryParams(
        queryParams.queryParams.map(q => if (q.id == query.id) query else q)
      ),
      modified = true
    )

  def updateFormField(field: FormField) =
    copy(
      body = body match
        case Body(BodyType.UrlEncodedForm(form)) =>
          Body(BodyType.UrlEncodedForm(Form(form.fields.map(f => if (f.id == field.id) field else f))))
        case Body(BodyType.MultipartForm(form)) =>
          Body(BodyType.MultipartForm(Form(form.fields.map(f => if (f.id == field.id) field else f))))
        case _ => body
      ,
      modified = true
    )

  def updateBodyRaw(body: String) =
    copy(
      body = Body(
        BodyType.Raw(body)
      ),
      modified = true
    )

  def unmodified = copy(modified = false)

  def send(env: Option[restui.Env]): ZIO[Client & Scope, Error, (ResponseBody, ResponseHeaders)] =
    val result = for {
      request <- ZIO.succeed(replaceEnv(env).getOrElse(this))
      client <- ZIO.service[Client]
      decodedUrl <- ZIO.fromEither(URL.decode(request.url.toString))
      httpMethod <- ZIO.succeed(method match
        case Method.Get =>
          zio.http.Method.GET
        case Method.Post =>
          zio.http.Method.POST
        case Method.Patch =>
          zio.http.Method.PATCH
        case Method.Put =>
          zio.http.Method.PUT
        case Method.Delete =>
          zio.http.Method.DELETE
        case Method.Head =>
          zio.http.Method.HEAD
      )
      start <- Clock.currentTime(TimeUnit.MILLISECONDS)
      res <- client
        .url(decodedUrl.addQueryParams(request.queryParams.toValue))
        .addHeaders(request.auth.auth.toHeader.map(request.headers.add(_)).getOrElse(request.headers).toValue)
        .request(httpMethod, "")(request.body.toValue)
      end <- Clock.currentTime(TimeUnit.MILLISECONDS)
      duration <- ZIO.succeed(end - start)
      bodyString <- res.body.asString
      headers <- ZIO.succeed(
        ResponseHeaders(headers =
          Headers(
            res.headers
              .map(h => Header(name = h.headerName, value = h.renderedValue))
              .flatten
              .toArray
          )
        )
      )
      status <- ZIO.succeed(ResponseStatus(res.status.code))
      body <- ZIO.succeed(
        ResponseBody(status = status, body = bodyString, headers = headers, duration = Duration(duration, MILLISECONDS))
      )
    } yield (body, headers)

    result.mapError(cause => Error.IOError(cause))

  def toCurlCommand(env: Option[restui.Env]) =
    val request = replaceEnv(env).getOrElse(this)
    List(
      "curl",
      s"-X $method",
      request.auth.toCurlCommand,
      request.headers.headers.map(_.toCurlCommand).mkString(" "),
      request.queryParams.queryParams.map(_.toCurlCommand).mkString(" "),
      request.body.toCurlCommand,
      request.url
    ).map(_.trim()).mkString(" ").replaceAll(" +", " ")

  private def replaceEnv(env: Option[restui.Env]) =
    for {
      e <- env
      url <- e.replace(url.toString).refineOption[RequestUrl]
      headers <- Some(headers.replaceEnv(e))
      queryParams <- Some(queryParams.replaceEnv(e))
      body <- Some(body.replaceEnv(e))
      auth <- Some(auth.replaceEnv(e))
    } yield this.copy(auth = auth, url = url, headers = headers, queryParams = queryParams, body = body)

object Request:
  implicit val encoder: JsonEncoder[Request] = DeriveJsonEncoder.gen[Request]
  implicit val decoder: JsonDecoder[Request] = RequestWithoutModified.decoder.map { requestWithoutModified =>
    Request(
      requestWithoutModified.id,
      requestWithoutModified.name,
      requestWithoutModified.headers,
      requestWithoutModified.queryParams,
      requestWithoutModified.body,
      requestWithoutModified.url,
      requestWithoutModified.method,
      requestWithoutModified.auth
    )
  }

  def create(name: String = "Untitled Request") = Request(
    java.util.UUID.randomUUID.toString.refine[Id],
    name.refine,
    Headers(Array()),
    QueryParams(Array()),
    Body(BodyType.NoBody),
    "http://localhost:3000".refine,
    Method.Get,
    Auth.noAuth
  )

private case class RequestWithoutModified(
    id: String :| Id,
    name: String :| RequestName,
    headers: Headers,
    queryParams: QueryParams,
    body: Body,
    url: String :| RequestUrl,
    method: Method,
    auth: Auth
)

private object RequestWithoutModified {
  implicit val decoder: JsonDecoder[RequestWithoutModified] = DeriveJsonDecoder.gen[RequestWithoutModified]
}
