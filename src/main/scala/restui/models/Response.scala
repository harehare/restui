package restui.models

import scala.concurrent.duration.Duration

import play.api.libs.json.Json

sealed abstract class Response

case class ResponseStatus(code: Int):
  override def toString =
    val text = code match
      case 100 => "Continue"
      case 101 => "Switching Protocols"
      case 102 => "Processing"
      case 200 => "Ok"
      case 201 => "Created"
      case 202 => "Accepted"
      case 203 => "Non-Authoritative Information"
      case 204 => "No Content"
      case 205 => "Reset Content"
      case 206 => "Partial Content"
      case 207 => "Multi Status"
      case 300 => "Multiple Choices"
      case 301 => "Moved Permanently"
      case 302 => "Found"
      case 303 => "See Other"
      case 304 => "Not Modified"
      case 305 => "Use Proxy"
      case 307 => "Temporary Redirect"
      case 308 => "Permanent Redirect"
      case 400 => "Bad Request"
      case 401 => "Unauthorized"
      case 402 => "Payment Required"
      case 403 => "Forbidden"
      case 404 => "Not Found"
      case 405 => "Method Not Allowed"
      case 406 => "Not Acceptable"
      case 407 => "Proxy Authentication Required"
      case 408 => "RequestTimeout"
      case 409 => "Conflict"
      case 410 => "Gone"
      case 411 => "Length Required"
      case 412 => "Precondition Failed"
      case 413 => "Request Entity Too Large"
      case 414 => "Request Uri Too Long"
      case 415 => "Unsupported Media Type"
      case 416 => "Requested Range Not Satisfiable"
      case 417 => "Expectation Failed"
      case 421 => "Misdirected Request"
      case 422 => "Unprocessable Entity"
      case 423 => "Locked"
      case 424 => "Failed Dependency"
      case 425 => "Unordered Collection"
      case 426 => "Upgrade Required"
      case 428 => "Precondition Required"
      case 429 => "Too Many Requests"
      case 431 => "Request Header Fields Too Large"
      case 500 => "Internal Server Error"
      case 501 => "Not Implemented"
      case 502 => "Bad Gateway"
      case 503 => "Service Unavailable"
      case 504 => "Gateway Timeout"
      case 505 => "Http Version Not Supported"
      case 506 => "Variant Also Negotiates"
      case 507 => "Insufficient Storage"
      case 510 => "Not Extended"
      case 511 => "Network Authentication Required"
      case _   => "Custom"
    s"${code.toString} ${text}"

enum ResponseBodyType:
  case Json
  case Xml
  case Html
  case Text
  case Auto(bodyType: Option[ResponseBodyType])

case class ResponseBody(
    status: ResponseStatus,
    body: String,
    duration: Duration,
    bodyType: ResponseBodyType
) extends Response:
  override def toString =
    bodyType match
      case ResponseBodyType.Json | ResponseBodyType.Auto(Some(ResponseBodyType.Json)) =>
        Json.prettyPrint(Json.parse(body))
      case ResponseBodyType.Xml | ResponseBodyType.Auto(Some(ResponseBodyType.Xml)) =>
        body
      case ResponseBodyType.Html | ResponseBodyType.Auto(Some(ResponseBodyType.Html)) =>
        body
      case ResponseBodyType.Text | ResponseBodyType.Auto(Some(ResponseBodyType.Text)) =>
        body
      case _ =>
        body

  def toDurationString =
    if (duration.toMinutes > 0) {
      s"${duration.toSeconds}s"
    } else if (duration.toSeconds > 0) {
      s"${(BigDecimal(duration.toMillis) / BigDecimal(1000)).setScale(1, scala.math.BigDecimal.RoundingMode.HALF_UP)}s"
    } else if (duration.toMillis > 0) {
      s"${duration.toMillis}ms"
    } else if (duration.toNanos > 0) {
      s"${duration.toNanos}ns"
    } else {
      "0"
    }

object ResponseBody:
  def apply(
      status: ResponseStatus,
      body: String,
      headers: ResponseHeaders,
      duration: Duration,
      bodyType: ResponseBodyType = ResponseBodyType.Auto(None)
  ): ResponseBody =
    bodyType match
      case ResponseBodyType.Json =>
        ResponseBody(status, body, duration, bodyType)
      case ResponseBodyType.Xml =>
        ResponseBody(status, body, duration, bodyType)
      case ResponseBodyType.Html =>
        ResponseBody(status, body, duration, bodyType)
      case ResponseBodyType.Text =>
        ResponseBody(status, body, duration, bodyType)
      case ResponseBodyType.Auto(_) =>
        ResponseBody(
          status,
          body,
          duration,
          ResponseBodyType.Auto(Some(headers.responseBodyType))
        )
  def toDisplayString = "Response"

case class ResponseHeaders(headers: Headers) extends Response:
  def responseBodyType =
    headers.headers
      .find(_.key.toLowerCase == "content-type")
      .map(h => h.value) match
      case Some(contentType) if contentType.contains("application/json") =>
        ResponseBodyType.Json
      case Some(contentType) if contentType.contains("application/xml") =>
        ResponseBodyType.Xml
      case Some(contentType) if contentType.contains("text/xml") =>
        ResponseBodyType.Xml
      case Some(contentType) if contentType.contains("text/html") =>
        ResponseBodyType.Html
      case Some(contentType) if contentType.contains("text/plain") =>
        ResponseBodyType.Text
      case _ => ResponseBodyType.Text

object ResponseHeaders:
  def empty =
    ResponseHeaders(headers = Headers(headers = Array()))
  def toDisplayString = "Headers"
