package zsync

object Main extends zio.App {
  import zio._
  import console._
  import zsync.config._
  import zsync.util._

  override def run(args: List[String]): URIO[ZEnv, ExitCode] = {
    def printConfig: ZIO[Console with Config, Nothing, Unit] =
      ZIO.access[Config](
        using(_)(identity)
      ) flatMap (c => putStrLn(s"${c.base} ${c.directories}"))

    printConfig.provideCustomLayer(configLive) foldM (err =>
      (err match {
        case _: NoSuchElementException =>
          putStrLnErr(
            s"Error while reading app config file: '${err.getMessage}'."
          )
        case _: NullPointerException =>
          putStrLnErr("Error while trying to open app config file.")
        case _ =>
          putStrLnErr("Unkown error occured while trying to read app config.")
      }) *> putStrLnErr("This is not an user error!") as ExitCode(1),
    _ => ZIO.unit as ExitCode(0))
  }
}
