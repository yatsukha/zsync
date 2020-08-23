package zsync

import zio._
import zio.blocking.Blocking
import console._
import nio.file.Files
import zsync.context._
import zsync.config._
import zsync.app.Add
import zsync.app.Remove
import zsync.app.Backup
import zsync.app.Help
import zsync.app.Action

object Main extends zio.App {

  // TODO: add interpreting of backup command

  def program(
    args: List[String]
  ): ZIO[Console with Blocking with Config with Context, Throwable, Unit] =
    Action
      .parse(args)
      .flatMap(Action.interpret)
      .foldM(
        err => putStrLnErr(err),
        _   => ZIO.unit
      )

  def printCommand(
    args: List[String]
  ): ZIO[Console with Blocking with Context, Nothing, Unit] =
    Action
      .parse(args)
      .foldM(
        err => putStrErr(err),
        cmd => putStrLn(cmd.toString())
      )

  lazy val bootstrap =
    Context.live ++ Console.live ++
      ((Context.live ++ Blocking.live) >>> Config.live)

  override def run(args: List[String]): URIO[ZEnv, ExitCode] =
    program(args)
      .provideCustomLayer(bootstrap)
      .foldM(
        err => putStrErr(s"Unrecoverable error: $err").as(ExitCode(1)),
        _   => ZIO.unit.as(ExitCode(0))
      )

}
