package restui.models
import java.nio.charset.StandardCharsets
import java.util.Base64

import io.github.iltotore.iron.constraint.all._
import zio._
import zio.json._

enum AuthType:
  case Basic(userName: String, password: String)
  case BearerToken(token: String)
  case NoAuth

  override def toString =
    this match
      case Basic(userName, password) =>
        if (userName.isEmpty() || password.isEmpty()) "" else s"${userName}:${password}"
      case BearerToken(token: String) =>
        token
      case NoAuth =>
        "No Auth"

  def toHeader =
    this match
      case Basic(userName, password) =>
        val encodedCredentials = Base64.getEncoder.encodeToString(s"$userName:$password".getBytes(StandardCharsets.UTF_8))
        Header(name = "Authorization", value = s"Basic $encodedCredentials")
      case BearerToken(token: String) =>
        Header(name = "Authorization", value = s"Bearer $token")
      case NoAuth =>
        None

  def toCurlCommand =
    this match
      case Basic(userName, password) =>
        s"-u '$userName:$password'"
      case BearerToken(token: String) =>
        s"-H 'Authorization:Bearer $token'"
      case NoAuth =>
        ""

  def replaceEnv(env: restui.Env) =
    this match
      case Basic(userName, password) =>
        Basic(userName = env.replace(userName.toString), password = env.replace(password.toString))
      case BearerToken(token: String) => BearerToken(env.replace(token.toString))
      case NoAuth                     => NoAuth

object AuthType:
  given JsonCodec[AuthType] = DeriveJsonCodec.gen
