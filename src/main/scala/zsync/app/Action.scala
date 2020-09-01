package zsync

import scala.collection.immutable.Nil
import zio.blocking.Blocking
import zio._
import zio.nio.core.file.Path
import zsync.context.Context
import zsync.config.Mode
import zsync.config.Recursive
import zsync.config.Git
import zsync.app.Add
import zsync.app.Remove
import zsync.app.Backup
import zsync.app.Help
import zio.console._
import zsync.config.Entry
import zsync.config.Config
import zio.nio.file.Files
import java.nio.file.FileVisitOption
import zio.process.Command

package object app {

  sealed trait Action {
    def interpret: ZIO[Blocking with Config with Console, Nothing, Unit] = ???
  }

  case class Add(mode: Mode, dirs: Set[Path]) extends Action {
    override def interpret
      : ZIO[Blocking with Config with Console, Nothing, Unit] =
      config
        .transformDirectories(entries => {
          var skipped: ZIO[Console, Nothing, Unit] = ZIO.unit
          var dirSet: Set[Path]                    = dirs

          entries.foreach(_ match {
            case Entry(p, m) =>
              if (dirSet.contains(p)) {
                dirSet -= p
                skipped *>= putStrLn(
                  s"$p was skipped since it already exists"
                )
              }
          })

          skipped
            .map(_ => entries ++ dirSet.map(p => Entry(p, mode)).toList)
        })
        .foldM(
          err =>
            putStrLnErr(
              s"error while adding given directories: ${err.getMessage}"
            ),
          _ => ZIO.unit
        )
  }

  case class Remove(dirs: Set[Path]) extends Action {
    override def interpret
      : ZIO[Blocking with Config with Console, Nothing, Unit] =
      config
        .transformDirectories(entries => {
          entries.partitionMap(e =>
            e match {
              case Entry(p, m) =>
                if (dirs.contains(p))
                  Left(putStrLn(s"$p was removed from the list"))
                else
                  Right(e)
            }
          ) match {
            case (outputs, entries) =>
              ZIO.collectAll(outputs) *> ZIO.succeed(entries)
          }
        })
        .foldM(
          err =>
            putStrLnErr(
              "error while removing given directories: ${err.getMessage}"
            ),
          _ => ZIO.unit
        )
  }

  case class Backup(dest: Path) extends Action {
    override def interpret
      : ZIO[Blocking with Config with Console, Nothing, Unit] =
      config
        .withDirectories(
          _.map({
            case Entry(path, mode) =>
              Files
                .exists(path)
                .flatMap(_ match {
                  case false => putStrErr(s"$path does not exist")
                  case true =>
                    Files
                      .isDirectory(path)
                      .flatMap(_ match {
                        case false => ???
                        case true =>
                          mode match {
                            case Recursive() =>
                              putStrLn(s"$path") *> Files
                                .walk(path)
                                .foreach(p => putStrLn(s" $p"))
                            case Git() =>
                              Command("git", "ls-files")
                                .workingDirectory(path.toFile)
                                .lines
                                .foldM(
                                  err =>
                                    putStrLnErr(
                                      s"error ocurred while running the git command (${err.getMessage}), " +
                                        s"are you sure $path is a git directory, and that git is installed?"
                                    ),
                                  putStrLn(s"git ls-files output for $path") *>
                                    _.map(ln => putStrLn(s" $ln"))
                                      .reduce(_ *> _)
                                )
                          }
                      })
                })
          }).reduce(_ *> _)
        )
        .foldM(
          err => putStrLnErr(s"error while backing up: ${err.getMessage}"),
          _   => ZIO.unit
        )
  }

  case class Help() extends Action {
    override def interpret
      : ZIO[Blocking with Config with Console, Nothing, Unit] =
      putStrLn(
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
        """.stripMargin.trim
      )
  }

  case class Ls() extends Action {
    override def interpret
      : ZIO[Blocking with Config with Console, Nothing, Unit] =
      config
        .withDirectories(
          _.map(entry => putStrLn(entry.toString)).reduce(_ *> _)
        )
        .foldM(
          err =>
            putStrLnErr(
              s"error while listing directories: ${err.getMessage}"
            ),
          _ => ZIO.unit
        )
  }

  object Action {
    def parse(
      args: List[String]
    ): ZIO[Blocking with Context, String, Action] =
      args match {
        case Nil => ZIO.succeed(Help())
        case cmd :: args =>
          cmd.toLowerCase match {
            case "add"    => Add.fromArgs(args)
            case "remove" => Remove.fromArgs(args)
            case "backup" => Backup.fromArgs(args)
            case "list"   => ZIO.succeed(Ls())
            case _        => ZIO.succeed(Help())
          }
      }
  }

  ///

  private sealed trait RequiresArgs {
    def fromArgs(
      paths: List[String]
    ): ZIO[Blocking with Context, String, Action]
  }

  private def expandPaths(
    paths: List[String]
  ): ZIO[Blocking with Context, String, List[Path]] = paths match {
    case head :: next =>
      expandPaths(next).foldM(
        err => ZIO.fail(err),
        next =>
          context
            .fromWorkingDirectory(head)
            .foldM(
              _    => ZIO.fail(s"'$head' could not be expanded into a valid path"),
              head => ZIO.succeed(head :: next)
            )
      )
    case Nil => ZIO.succeed(List())
  }

  private object Add extends RequiresArgs {
    private lazy val syntaxErr = ZIO.fail(
      "'add' requires at least two arguments, "
        + "mode ('git' or 'recursive') and at least one directory path"
    )

    override def fromArgs(
      args: List[String]
    ): ZIO[Blocking with Context, String, Action] = args match {
      case Nil => syntaxErr
      case mode :: args =>
        (mode.toLowerCase match {
          case "git"       => ZIO.succeed(Git())
          case "recursive" => ZIO.succeed(Recursive())
          case _           => syntaxErr
        }).flatMap(m =>
          args match {
            case Nil => syntaxErr
            case _   => expandPaths(args).map(paths => Add(m, paths.toSet))
          }
        )
    }
  }

  private object Remove extends RequiresArgs {
    override def fromArgs(
      args: List[String]
    ): ZIO[Blocking with Context, String, Action] = args match {
      case Nil => ZIO.fail("'remove' requires at least one directory path")
      case _   => expandPaths(args).map(paths => Remove(paths.toSet))
    }
  }

  private object Backup extends RequiresArgs {
    override def fromArgs(
      args: List[String]
    ): ZIO[Blocking with Context, String, Action] = args match {
      case Nil => ZIO.fail("'backup' requires a destination folder")
      case _   => expandPaths(args).map(paths => Backup(paths.head))
    }
  }

}
