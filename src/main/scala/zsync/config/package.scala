package zsync

package object config {
  import zio._
  import zio.nio.core.file.Path

  trait Service {
    def base: Path
    def directories: Path
  }

  type Config = Has[Service]

  val configLive: ZLayer[system.System, Throwable, Config] = ZLayer.fromEffect(
    system.env("HOME") flatMap (
      homeOpt =>
        ZManaged
          .makeEffect(util.nonNull(getClass.getResourceAsStream("/config")))(
            _.close()
          )
          .use(
            stream =>
              ZIO.effect {
                val map = scala.io.Source
                  .fromInputStream(stream)
                  .getLines
                  .map(_.trim)
                  .filter(_.contains("="))
                  .map(s => {
                    val i = s.indexOf("=")
                    (s take i, s drop (i + 1))
                  })
                  .toMap

                val home = homeOpt match {
                  case Some(value) => value
                  case None        => "~"
                }

                (
                  Path(map("base").replace("~", home)).normalize,
                  Path(map("directories").replace("~", home)).normalize
                ) match {
                  case (b, d) =>
                    new Service {
                      override def base: Path        = b
                      override def directories: Path = d
                    }
                }
              }
          )
      )
  )

}
