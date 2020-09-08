package zsync

import zio._
import zio.blocking.Blocking
import console.{putStr => _, putStrErr => _, _}
import zsync.context._
import zsync.config._
import zsync.app._

object Main extends zio.App {

  // TODO:
  //  - test zip refactor

  def program(
    args: List[String]
  ): ZIO[Console with Blocking with Config with Context, Throwable, Unit] =
    Action
      .parse(args)
      .flatMap(_.interpret)
      .foldM(
        err => putStrLnErr(err),
        _   => ZIO.unit
      )

  lazy val dependencies =
    Context.live ++ ((Context.live ++ Blocking.live) >>> Config.live)

  override def run(args: List[String]): URIO[ZEnv, ExitCode] =
    program(args)
      .provideCustomLayer(dependencies)
      .foldM(
        err =>
          putStrLnErr(s"Unrecoverable error: ${err.getMessage}") *>
            putStrLnErr(err.getStackTrace.map(_.toString).mkString("\n"))
              .as(ExitCode(1)),
        _ => ZIO.succeed(ExitCode(0))
      )

}
