package zsync

import zio.{config => _, _}
import zio.blocking.Blocking
import console._
import nio.file.Files
import zsync.context._
import zsync.config._
import zsync.app.Add
import zsync.app.Remove
import zsync.app.Backup
import zsync.app.Help

object Main extends zio.App {

  lazy val help: ZIO[Console, Nothing, Unit] = putStrLn(
    """
    |zsync 0.0.1 - a lacking rsync clone written using ZIO
    |
    |usage:
    |  zsync <action> [argument...]
    |
    |action can be any of the following, with respective arguments:
    |
    |  help
    |    - outputs this text
    |
    |  add <method> <directories...>
    |    - ads the given directories to the list of directories that 
    |      should backed up optionally specifying the method
    |    - method can be 'recursive' which backups 
    |      all files in the given directory, or 'git' which uses git 
    |      ls-files to get the list of files to backup, while also 
    |      adding the .git subdirectory
    |
    |  remove <directories...> 
    |    - removes the given directories from the backup list
    |  
    |  backup <destination>
    |    - backs up files to given destination
    |    - does not replicate directory structure, instead each file is in the
    |      form of "/home/user/some/path".zip
    |    - files that were already backed up are skipped
  """.stripMargin
  )

  // TODO: add interpreting of commands
  // TODO: add updating of backup list

  def program(
    args: List[String]
  ): ZIO[Console with Blocking with Config, Throwable, Unit] =
    config.exists
      .flatMap(_ match {
        case true  => putStrLn("picked up user config")    *> config.read
        case false => putStrLn("no user config, creating") *> config.create
      })
      .as(())

  def printCommand(
    args: List[String]
  ): ZIO[Console with Blocking with Context, Nothing, Unit] =
      app.parse(args).foldM(
        err => putStrErr(err),
        cmd => putStrLn(cmd.toString())
      )

  lazy val bootstrap = (Context.live ++ Blocking.live) >>> Config.live

  override def run(args: List[String]): URIO[ZEnv, ExitCode] =
    printCommand(args)
      .provideCustomLayer(Context.live)
      .foldM(
        err => putStrErr(s"Unrecoverable error: $err").as(ExitCode(1)),
        _   => ZIO.unit.as(ExitCode(0))
      )
}
