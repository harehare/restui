package restui

import java.time.Duration

import restui.models.{App, Collection}
import restui.views.StatefulList
import tui._
import zio._
import zio.cli.HelpDoc.Span.text
import zio.cli.{Args, _}

object Main extends ZIOCliDefault {
  val config = Options.WithDefault(Options.file("config", Exists.Yes).alias("c"), models.Config.DefaultPath)
  val dotenv = Options.file("env", Exists.Yes).alias("e").optional
  val command = Command("restui", config ++ dotenv, Args.none)

  override def cliApp = CliApp.make(
    name = "ResTui",
    version = "0.1.0",
    summary = text("ResTui is http request management tool"),
    command = command
  ) { case ((config, dotenv)) =>
    withTerminal { (jni, terminal) =>
      for {
        items <- models.Config(Some(config.toString)).read
        collectionList <- ZIO.succeed(
          StatefulList(items = items).select
        )
        requests <- ZIO.fromOption(
          collectionList.current.map(_.requests)
        )
        app <- ZIO.succeed(
          App(
            terminal = terminal,
            render = ui.Main.render,
            currentLocation = models.Location.Collection,
            collectionList = collectionList,
            requestTab = models.Tabs(requests),
            requestView = requests.lift(0).map(models.RequestView(_)).get,
            responseView = None,
            configPath = Some(config.toString),
            env = dotenv.map(filepath => restui.Env.load(filepath.toString))
          )
        )
        _ <- ZIO.succeed(terminal.clear())
        _ <- ZIO.succeed(jni.enableRawMode())
        exitCode <- app
          .run(
            jni,
            Duration.ofMillis(100)
          )
          .map(_ match {
            case Some(Exit.Success(_)) => ExitCode.success
            case Some(Exit.Failure(_)) => ExitCode.failure
            case _                     => ExitCode.success
          })
        _ <- ZIO.succeed(terminal.showCursor())
        _ <- ZIO.succeed(terminal.flush())
        _ <- ZIO.succeed(jni.disableRawMode())
      } yield exitCode
    }
  }
}
