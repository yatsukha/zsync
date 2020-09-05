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
import java.util.zip.ZipOutputStream
import java.io.FileOutputStream
import java.util.zip.ZipEntry
import java.io.BufferedInputStream
import java.io.FileInputStream
import java.nio.file.attribute.FileAttribute
import java.nio.file.attribute.BasicFileAttributes
import zio.stream.ZStream

package object app {

  sealed trait Action {
    def interpret
      : ZIO[Blocking with Console with Config, Nothing, Unit] = ???
  }

  case class Add(mode: Mode, dirs: Set[Path]) extends Action {
    override def interpret
      : ZIO[Blocking with Console with Config, Nothing, Unit] =
      config
        .transformDirectories(entries =>
          ZIO.effect {
            var notifiers: ZIO[Console, Nothing, Unit] = ZIO.unit
            var dirSet: Set[Path]                      = dirs
            var updSet: Set[Path]                      = Set()

            entries.foreach(_ match {
              case Entry(p, m) =>
                if (dirSet.contains(p)) {
                  if (m != mode) {
                    updSet += p
                    notifiers *>= putStrLn(
                      s"$p backup mode updated to ${m.toString.toLowerCase}"
                    )
                  } else {
                    dirSet -= p
                    notifiers *>= putStrLn(
                      s"$p was skipped since it already exists"
                    )
                  }
                }
            })

            notifiers
              .map(_ =>
                entries.filter(e => !updSet.contains(e.path)) ++ dirSet
                  .map(p => Entry(p, mode))
                  .toList
              )
          }.flatten
        )
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
      : ZIO[Blocking with Console with Config, Nothing, Unit] =
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
      : ZIO[Blocking with Console with Config, Nothing, Unit] =
      config
        .withDirectories(
          _.map({
            case Entry(path, mode) =>
              Files
                .exists(path)
                .flatMap(_ match {
                  case false => putStrLnErr(s"$path does not exist")
                  case true =>
                    Files
                      .isDirectory(path)
                      .flatMap(_ match {
                        case false => Backup.backupFile(dest, path)
                        case true =>
                          mode match {
                            case Recursive() =>
                              Backup.backupRecursive(dest, path)
                            case Git() => Backup.backupGit(dest, path)
                          }
                      })
                      .foldM(
                        err => putStrLnErr(err),
                        _   => putStrLn(s"$path backed up")
                      )
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
        |    - method can be 'recursive' which 
        |      all files in the given directory, or 'git' which uses git 
        |      ls-files to get the list of files to backup, while also 
        |      adding the .git subdirectory
        |
        |  remove <directories...> 
        |    - removes the given directories from the backup list
        |  
        |  backup <destination>
        |    - backs up files to the given destination
        |    - folder structure is preserved
        |    - files that have up to date backup are skipped
        """.stripMargin.trim
      )
  }

  case class Ls() extends Action {
    override def interpret
      : ZIO[Blocking with Config with Console with Context, Nothing, Unit] =
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

    private def backupFile(
      backupPath: Path,
      file: Path
    ): ZIO[Blocking, String, Unit] =
      for {
        parent <- file.parent match {
          case Some(parent) => ZIO.succeed(parent)
          case None         => ZIO.fail(s"$file is in root directory")
        }
        _ <- zip(destinationFrom(backupPath, file), parent, Seq(file))
      } yield ()

    private def backupRecursive(
      backupPath: Path,
      dir: Path
    ): ZIO[Blocking, String, Unit] =
      for {
        files <- tree(dir)
        _     <- zip(destinationFrom(backupPath, dir), dir, files)
      } yield ()

    private def backupGit(
      backupPath: Path,
      dir: Path
    ): ZIO[Blocking, String, Unit] =
      (for {
        tracked <- Command("git", "ls-files").workingDirectory(dir.toFile).lines
        dotGit  <- tree(dir / ".git")
        _ <- zip(
          destinationFrom(backupPath, dir),
          dir,
          tracked.map(dir / _) ++ dotGit
        )
      } yield ()).fold(
        err => s"$dir not a git directory, or git is not installed",
        identity
      )

    private def destinationFrom(backupPath: Path, directoryPath: Path): Path =
      Path(backupPath.toString + directoryPath.toString) / Path("base.zip")

    private def tree(base: Path): ZIO[Blocking, Nothing, List[Path]] =
      Files
        .walk(base)
        .filterM(p => Files.isDirectory(p).map(!_))
        .runCollect
        .map(_.toList)
        .orDie

    private def zip(
      out: Path,
      base: Path,
      files: Seq[Path]
    ): ZIO[Blocking, String, Unit] =
      (
        ZIO
          .effect(out.parent match {
            case None          => ZIO.unit
            case Some(parents) => Files.createDirectories(parents)
          })
          .flatten *>
          Files.createFile(out)
      ).fold(err => s"failed while creating backup tree $err", identity) *>
        ZManaged
          .makeEffect(
            new ZipOutputStream(
              new FileOutputStream(out.toFile)
            )
          )(
            _.close()
          )
          .use(zos =>
            ZIO.effect { // leaky abstractions ahead :^)
              val buffer = Array.ofDim[Byte](8096)

              files.foreach(file => {
                val in =
                  new BufferedInputStream(new FileInputStream(file.toFile))
                zos.putNextEntry(new ZipEntry(base.relativize(file).toString))
                var read = in.read(buffer, 0, buffer.length)
                while (read != -1) {
                  zos.write(buffer, 0, read)
                  read = in.read(buffer, 0, buffer.length)
                }
                in.close()
                zos.closeEntry()
              })
            }
          )
          .fold(err => s"failed while zipping: ${err.getMessage}", identity)
  }

}
