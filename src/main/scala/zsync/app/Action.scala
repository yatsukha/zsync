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

  case class Add(dir: Path, mode: Mode) extends Action
  case class Remove(dir: Path)          extends Action
  case class Backup(dest: Path)         extends Action
  case class Help()                     extends Action

  def parse(
    args: List[String]
  ): ZIO[Blocking with Context, Throwable, Option[Action]] =
    ZIO.unit.flatMap(_ =>
      args match {
        case cmd :: next =>
          next match {
            case arg :: next =>
              context
                .fromWorkingDirectory(arg)
                .map(pArg =>
                  cmd.toLowerCase match {
                    case "add" =>
                      Some(next match {
                        case m :: next =>
                          m.toLowerCase match {
                            case "git"       => Add(pArg, Git())
                            case "recursive" => Add(pArg, Recursive())
                            case _           => Help()
                          }
                        case Nil => Add(pArg, Recursive())
                      })
                    case "remove" => Some(Remove(pArg))
                    case "backup" => Some(Backup(pArg))
                    case _        => Some(Help())
                  }
                )
            case Nil => ZIO.succeed(Some(Help()))
          }
        case Nil => ZIO.succeed(None)
      }
    )

}
