package zsync

import zio.{config => _, _}
import zio.blocking.Blocking
import console._
import nio.file.Files

object Main extends zio.App {

  lazy val bootstrap = (context.live ++ Blocking.live) >>> config.live

  val program
    : ZIO[Console with Blocking with config.ConfigPaths, Throwable, Unit] =
    config.exists
      .flatMap(_ match {
        case true  => putStrLn("Picked up user config.")    *> config.read
        case false => putStrLn("No user config, creating.") *> config.create
      })
      .as(ExitCode(0))

  def printCommand(args: List[String]) =
    app
      .parse(args)
      .foldM(
        _ => putStrLnErr("Error while expanding path arguments."),
        _ match {
          case Some(value) => putStrLn("Command: " + value.toString)
          case None        => putStrLn("Invalid command.") // TODO: add help
        }
      )

  def printConfig: ZIO[Console with config.ConfigPaths, Nothing, Unit] =
    ZIO
      .access[config.ConfigPaths](identity)
      .flatMap(hasCfg => {
        val cfg = hasCfg.get
        putStrLn(s"${cfg.base} ${cfg.directories}")
      })

  override def run(args: List[String]): URIO[ZEnv, ExitCode] =
    printConfig
      .provideCustomLayer(bootstrap)
      .foldM(
        err => putStrErr(s"Unrecoverable error: $err").as(ExitCode(1)),
        _   => ZIO.unit.as(ExitCode(0))
      )
}
