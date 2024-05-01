package restui.models
import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}

import scala.io.Source

import zio.ZIO
import zio.json._

case class Config(filePath: Option[String]):
  def read: ZIO[Any, Throwable | String, Array[Collection]] =
    ZIO.scoped {
      for {
        path <- ZIO.succeed(filePath.getOrElse(Config.DefaultPath.toString()))
        items <- ZIO.ifZIO(ZIO.succeed(new File(path).exists()))(
          onTrue = for {
            configJsonFile <- ZIO.fromAutoCloseable(
              ZIO.attempt(
                Source.fromFile(filePath.getOrElse(Config.DefaultPath.toString()))
              )
            )
            configJson <- ZIO.succeed(configJsonFile.getLines.mkString)
            items <- ZIO.fromEither(configJson.fromJson[Array[Collection]])
          } yield items,
          onFalse = write(Array(Collection.init))
        )
      } yield items
    }

  def write(collections: Array[Collection]) =
    ZIO.scoped {
      for {
        _ <- ZIO.attempt(Files.createDirectories(Paths.get(filePath.getOrElse(Config.DefaultPath.toString())).getParent()))
        writer <- ZIO.fromAutoCloseable(
          ZIO.attempt(new PrintWriter(filePath.getOrElse(Config.DefaultPath.toString())))
        )
        _ <- ZIO.attempt {
          writer.write(collections.toJson)
        }
      } yield collections
    }

object Config:
  val DefaultPath = Paths.get(sys.env.getOrElse("HOME", "./")).resolve(Paths.get(".config/restui/collections.json"))
