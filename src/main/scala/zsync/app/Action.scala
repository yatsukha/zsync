package zsync

import scala.collection.immutable.Nil
import zio.blocking.Blocking
import zio._
import zio.nio.core.file.Path
import zsync.context.Context
import zsync.config.Mode
import zsync.config.Recursive
import zsync.config.Git

package object app {

  sealed trait Action

  case class Add(mode: Mode, dirs: List[Path]) extends Action
  case class Remove(dirs: List[Path])          extends Action
  case class Backup(dest: Path)                extends Action
  case class Help()                            extends Action

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
            case _        => ZIO.succeed(Help())
          }
      }

    def interpret(a: Action) = ???
  }

  ///

  private sealed trait RequiresArgs {
    def fromArgs(
      paths: List[String]
    ): ZIO[Blocking with Context, String, Action]

    def expandPaths(
      paths: List[String]
    ): ZIO[Blocking with Context, String, List[Path]] = paths match {
      case head :: next =>
        expandPaths(next).foldM(
          err => ZIO.fail(err),
          next =>
            context
              .fromWorkingDirectory(head)
              .foldM(
                _ =>
                  ZIO.fail(s"'$head' could not be expanded into a valid path"),
                head => ZIO.succeed(head :: next)
              )
        )
      case Nil => ZIO.succeed(List())
    }
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
            case _   => expandPaths(args).map(paths => Add(m, paths))
          }
        )
    }
  }

  private object Remove extends RequiresArgs {
    override def fromArgs(
      args: List[String]
    ): ZIO[Blocking with Context, String, Action] = args match {
      case Nil => ZIO.fail("'remove' requires at least one directory path")
      case _   => expandPaths(args).map(paths => Remove(paths))
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
