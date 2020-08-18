package zsync

import zio.nio.core.file.Path
import zio.Has
import zio.ZLayer
import zio._
import zio.blocking.Blocking

package object context {

  trait Service {
    val home: Path
    val workingDirectory: Path
  }

  type Context = Has[Service]

  object Context {
    val live: ZLayer[system.System, Throwable, Context] =
      ZLayer.fromEffect(
        system
          .env("HOME")
          .flatMap(_ match {
            case None => ZIO.fail(new RuntimeException("No $HOME in path."))
            case Some(h) =>
              system
                .property("user.dir")
                .flatMap(_ match {
                  case None =>
                    ZIO.fail(
                      new RuntimeException("Can not deduce working directory.")
                    )
                  case Some(wd) =>
                    ZIO.succeed(new Service {
                      override val home: Path             = Path(h)
                      override val workingDirectory: Path = Path(wd)
                    })
                })
          })
      )
  }

  def toNormalizedPath(p: String): ZIO[Blocking with Context, Throwable, Path] =
    ZIO
      .access[Context](_.get)
      .map(ctx => Path(p.replace("~", ctx.home.toString)).normalize)

  def fromWorkingDirectory(
    p: String
  ): ZIO[Blocking with Context, Throwable, Path] =
    ZIO
      .access[Context](_.get)
      .map(ctx => (ctx.workingDirectory / p).normalize)

}
