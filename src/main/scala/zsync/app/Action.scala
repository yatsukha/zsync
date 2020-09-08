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
    def interpret: ZIO[Blocking with Console with Config, Nothing, Unit] = ???
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
        |zsync 0.1.0 - a lacking rsync clone written using ZIO
        |            - see https://zio.dev/
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
        |    - adds the given directories (or files) to the list of directories
        |      that should be backed up
        |    - method can be 'recursive' which backs up
        |      all files in the given directory, or 'git' which uses git 
        |      ls-files to get the list of files to backup, while also 
        |      adding the .git subdirectory to preserve history
        |
        |  list
        |    - lists directories and their backup methods
        |
        |  remove <directories...> 
        |    - removes the given directories from the backup list
        |  
        |  backup <destination>
        |    - backs up files to the given destination using zip archives
        |    - if the destination folder does not exist it will be created
        |    - folder structures are preserved
        |    - files that are up to date backup are skipped
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

  /// messy implementations :^)

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
      for {
        files <- Command("git", "ls-files")
          .workingDirectory(dir.toFile)
          .lines
          .map(_.map(dir / _).toList)
          .mapError(_ =>
            s"$dir is not a git repository or git is not installed"
          )
        dotGit <- tree(dir / ".git")
        _      <- zip(destinationFrom(backupPath, dir), dir, files ++ dotGit)
      } yield ()

    private def isUpToDate(
      backupFile: Path,
      paths: Seq[Path]
    ): ZIO[Blocking, Nothing, Boolean] =
      Files
        .exists(backupFile)
        .flatMap(_ match {
          case false => ZIO.succeed(false)
          case true =>
            for {
              lastModified <- ZIO.succeed(backupFile.toFile.lastModified)
              noNewer = paths.find(_.toFile.lastModified > lastModified).isEmpty
            } yield noNewer
        })

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
      for {
        upToDate <- isUpToDate(out, files)
        _ <- upToDate match {
          case true  => ZIO.fail(s"${
            if (files.size == 1)
              files.head
            else
              base
          } is already up to date")
          case false => ZIO.unit
        }

        exists <- Files.exists(out)
        fileCreation = exists match {
          case true => ZIO.unit
          case false =>
            (out.parent match {
              case None         => ZIO.unit
              case Some(parent) => Files.createDirectories(parent)
            }) *> Files.createFile(out)
        }
        _ <- fileCreation.mapError(_ =>
          s"$base failed while creating backup tree"
        )

        buffer = Array.ofDim[Byte](8192)
        canFail = ZManaged
          .makeEffect(
            new ZipOutputStream(new FileOutputStream(out.toFile))
          )(_.close())
          .use(zos =>
            ZIO.collectAll(
              files.map(p =>
                (ZManaged.makeEffect(
                  new BufferedInputStream(new FileInputStream(p.toFile))
                )(_.close()) <*> ZManaged.makeEffect(
                  zos.putNextEntry(new ZipEntry(base.relativize(p).toString))
                )(_ => zos.closeEntry())).use({
                  case (inputStream, _) =>
                    ZIO.effect {
                      var read = inputStream.read(buffer, 0, buffer.length)
                      while (read != -1) {
                        zos.write(buffer, 0, read)
                        read = inputStream.read(buffer, 0, buffer.length)
                      }
                    }
                })
              )
            )
          )
        _ <- canFail.mapError(_ =>
          s"$base failed while zipping, archive might be in an incomplete state"
        )
      } yield ()
  }

}
