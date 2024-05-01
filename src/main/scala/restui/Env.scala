package restui

import java.io.File

import scala.collection.JavaConverters._

import io.github.cdimascio.dotenv.Dotenv

case class Env(entries: Map[String, String]):
  def replace(value: String) =
    entries.keys.foldLeft(value)((acc, key) => entries.get(key).map(value => acc.replace(s"$${${key}}", value)).getOrElse(acc))

object Env:
  def load(filepath: String) =
    val file = new File(filepath)
    if (file.getParent() == null) {
      Env(
        Dotenv
          .configure()
          .filename(file.getName())
          .load()
          .entries()
          .asScala
          .map(v => (v.getKey(), v.getValue()))
          .toMap
      )
    } else {
      Env(
        Dotenv
          .configure()
          .directory(file.getParent())
          .filename(file.getName())
          .load()
          .entries()
          .asScala
          .map(v => (v.getKey(), v.getValue()))
          .toMap
      )
    }
