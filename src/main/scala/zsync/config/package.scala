package zsync

import zio._
import zio.console._
import zio.nio.core.file.Path
import zio.blocking.Blocking
import zsync.context.Context
import java.nio.file.OpenOption
import java.nio.file.StandardOpenOption
import zio.nio.core.charset.Charset.Standard
import util._
import nio.file.Files

package object config {

  trait Service {
    val base: Path
    val directories: Path
  }

  type Config = Has[Service]

  object Config {
    val live: ZLayer[Blocking with Context, Throwable, Config] =
      ZLayer.fromEffect(
        ZManaged
          .makeEffect(
            util
              .nonNull(getClass.getResourceAsStream("/app.config"))
          )(
            _.close()
          )
          .use(stream =>
            ZIO
              .effect {
                val map = scala.io.Source
                  .fromInputStream(stream)
                  .getLines
                  .map(_.trim)
                  .filter(_.contains("="))
                  .map(s => {
                    val i = s.indexOf("=")
                    (s.take(i), s.drop(i + 1))
                  })
                  .toMap

                (map("base"), map("directories"))
              }
              .flatMap(_ match {
                case (b, d) =>
                  context.toNormalizedPath(b) <*> context.toNormalizedPath(d)
              })
              .map(_ match {
                case (b, d) =>
                  new Service {
                    override val base: Path        = b
                    override val directories: Path = b / d
                  }
              })
          )
      )
  }

  type Entries = List[Entry]

  def transformDirectories(
    f: Entries => ZIO[Blocking with Console, Throwable, Entries]
  ): ZIO[Console with Blocking with Config, Throwable, Unit] =
    for {
      e <- exists
      entries <- e match {
        case true  => read
        case false => create
      }
      modified <- f(entries)
      _        <- persist(modified)
    } yield ()

  def withDirectories(
    f: Entries => ZIO[Blocking with Console, Throwable, Unit]
  ): ZIO[Console with Blocking with Config, Throwable, Unit] =
    for {
      e <- exists
      entries <- e match {
        case true  => read
        case false => create
      }
      _ <- f(entries)
    } yield ()

  private def exists: ZIO[Blocking with Config, Nothing, Boolean] =
    ZIO.access[Config](_.get).flatMap(c => Files.exists(c.directories))

  private def read: ZIO[Blocking with Config, Throwable, Entries] =
    ZIO
      .access[Config](_.get)
      .flatMap(c => Files.readAllLines(c.directories))
      .flatMap(e =>
        ZIO.collectAll(
          e.map(_.trim)
            .filter(!_.isEmpty)
            .map(Entry.parse)
        )
      )

  private def create: ZIO[Blocking with Config, Throwable, Entries] =
    ZIO
      .access[Config](_.get)
      .flatMap(c =>
        (c.directories.parent match {
          case Some(p) => Files.createDirectories(p)
          case None    => ZIO.unit
        }) *>
          Files.createFile(c.directories) *>
          ZIO.succeed(List())
      )

  private def persist(e: Entries): ZIO[Blocking with Config, Throwable, Unit] =
    for {
      config <- ZIO.access[Config](_.get)
      _ <- Files.writeLines(
        config.directories,
        e.map(_.toString),
        openOptions =
          Set(StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
      )
    } yield ()

}
