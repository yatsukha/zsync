import zio.console._
import zio.ExitCode

object Main extends zio.App {

  override def run(args: List[String]): zio.URIO[zio.ZEnv, ExitCode] =
    (
      for {
        _    <- putStrLn("Hello world!")
        name <- getStrLn
        _    <- putStrLn(s"My name is $name!")
      } yield ()
    ).exitCode

}
