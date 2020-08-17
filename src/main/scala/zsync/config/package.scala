package zsync

package object config {
  import zio._
  import zio.nio.core.file.Path
  import zio.blocking.Blocking

  trait Service {
    val base: Path
    val directories: Path
  }

  type ConfigPaths = Has[Service]

  val live: ZLayer[Blocking with context.Context, Throwable, ConfigPaths] =
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

  import util._
  import nio.file.Files

  def exists: ZIO[Blocking with ConfigPaths, Nothing, Boolean] =
    ZIO.access[ConfigPaths](_.get).flatMap(c => Files.exists(c.directories))

  type Paths = List[Path]

  private val commentPrefix = ":"

  def read: ZIO[Blocking with ConfigPaths, Throwable, Paths] =
    ZIO
      .access[ConfigPaths](_.get)
      .flatMap(c => Files.readAllLines(c.directories))
      .map(
        _.map(_.trim)
          .filter(l => l.size > 0 && !l.startsWith(commentPrefix))
          .map(Path(_))
      )

  def create: ZIO[Blocking with ConfigPaths, Throwable, Paths] =
    ZIO
      .access[ConfigPaths](_.get)
      .flatMap(c =>
        (c.directories.parent match {
          case Some(p) => Files.createDirectories(p)
          case None    => ZIO.unit
        }) *>
          Files.createFile(c.directories) *>
          Files.writeLines(
            c.directories,
            List(
              commentPrefix +
                " file created and managed by zsync, do not edit manually!"
            )
          ) *>
          ZIO.succeed(List())
      )

}
