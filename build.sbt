val scala3Version = "3.4.1"

lazy val root = project
  .in(file("."))
  .enablePlugins(JavaAppPackaging)
  .enablePlugins(GraalVMNativeImagePlugin)
  .settings(
    name := "restui",
    version := "0.1.0",
    scalaVersion := scala3Version,
    Compile / mainClass := Some("restui.Main"),
    assembly / assemblyJarName := "restui.jar",
    graalVMNativeImageOptions ++= List(
      "--verbose",
      "--no-fallback",
      "-H:+ReportExceptionStackTraces",
      "--initialize-at-build-time=scala.runtime.Statics$VM",
      "--initialize-at-build-time=scala.Symbol",
      "--initialize-at-build-time=scala.Symbol$",
      "--native-image-info",
      """-H:IncludeResources=libnative-arm64-darwin-crossterm.dylib""",
      """-H:IncludeResources=libnative-x86_64-darwin-crossterm.dylib""",
      """-H:IncludeResources=libnative-x86_64-linux-crossterm.so""",
      """-H:IncludeResources=native-x86_64-windows-crossterm.dll""",
      "-H:-UseServiceLoaderFeature",
      "--initialize-at-run-time=io.netty.incubator.channel.uring.IOUringEventLoopGroup",
      "--initialize-at-run-time=io.netty.incubator.channel.uring.Native",
      "--initialize-at-run-time=io.netty.channel.epoll.Epoll",
      "--initialize-at-run-time=io.netty.channel.epoll.Native",
      "--initialize-at-run-time=io.netty.channel.epoll.EpollEventLoop",
      "--initialize-at-run-time=io.netty.channel.epoll.EpollEventArray",
      "--initialize-at-run-time=io.netty.channel.DefaultFileRegion",
      "--initialize-at-run-time=io.netty.channel.kqueue.KQueueEventArray",
      "--initialize-at-run-time=io.netty.channel.kqueue.KQueueEventLoop",
      "--initialize-at-run-time=io.netty.channel.kqueue.Native",
      "--initialize-at-run-time=io.netty.channel.unix.Errors",
      "--initialize-at-run-time=io.netty.channel.unix.IovArray",
      "--initialize-at-run-time=io.netty.channel.unix.Limits",
      "--initialize-at-run-time=io.netty.util.internal.logging.Log4JLogger",
      "--initialize-at-run-time=io.netty.util.AbstractReferenceCounted",
      "--initialize-at-run-time=io.netty.channel.kqueue.KQueue",
      "--initialize-at-build-time=org.slf4j.LoggerFactory",
      "--initialize-at-build-time=org.slf4j.simple.SimpleLogger",
      "--initialize-at-build-time=io.netty.util.internal.logging.Slf4JLoggerFactory",
      "--initialize-at-build-time=io.netty.channel.MultithreadEventLoopGroup",
      "--initialize-at-build-time=io.netty.util.internal.logging.InternalLoggerFactory",
      "--initialize-at-run-time=io.netty.handler.ssl.BouncyCastleAlpnSslUtils",
      "--initialize-at-run-time=io.netty.incubator.codec.quic.InsecureQuicTokenHandler",
      "--initialize-at-run-time=io.netty.incubator.codec.quic.SecureRandomQuicConnectionIdGenerator",
      "--initialize-at-run-time=io.netty.util.internal.logging.Slf4JLoggerFactory$NopInstanceHolder",
      "--initialize-at-run-time=io.netty.handler.ssl.ReferenceCountedOpenSslContext",
      "--initialize-at-run-time=io.netty.handler.ssl.ReferenceCountedOpenSslEngine"
    ),
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "dev.zio" %% "zio-test" % "2.0.19" % Test,
      "dev.zio" %% "zio-cli" % "0.5.0",
      "dev.zio" %% "zio-http" % "3.0.0-RC3",
      "dev.zio" %% "zio-json" % "0.6.2",
      "dev.zio" %% "zio" % "2.0.19",
      "io.github.iltotore" %% "iron" % "2.4.0",
      "io.github.iltotore" %% "iron-zio" % "2.4.0",
      "io.github.iltotore" %% "iron-zio-json" % "2.4.0",
      "dev.optics" %% "monocle-core" % "3.1.0",
      "dev.optics" %% "monocle-macro" % "3.1.0",
      "com.olvind.tui" % "tui_3" % "0.0.7",
      "org.playframework" %% "play-json" % "3.0.2",
      "io.github.cdimascio" % "dotenv-java" % "3.0.0",
      "org.scalatest" %% "scalatest" % "3.2.17" % "test",
      "org.scalatestplus" %% "scalacheck-1-17" % "3.2.17.0" % "test"
    ),
    ThisBuild / assemblyMergeStrategy := {
      case PathList(ps @ _*) if ps.last endsWith ".html"       => MergeStrategy.first
      case PathList(ps @ _*) if ps.last endsWith ".properties" => MergeStrategy.first
      case PathList(ps @ _*) if ps.last endsWith ".class"      => MergeStrategy.first
      case "application.conf"                                  => MergeStrategy.concat
      case "unwanted.txt"                                      => MergeStrategy.discard
      case x =>
        val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
        oldStrategy(x)
    }
  )

scalacOptions := Seq(
  "-Wunused:imports"
)
