package zsync

import scala.collection.immutable.Nil
import zio.blocking.Blocking
import zio._
import zio.nio.core.file.Path

package object app {

  sealed trait Action

  case class Add(dir: Path)     extends Action
  case class Remove(dir: Path)  extends Action
  case class Backup(dest: Path) extends Action

  def parse(
    args: List[String]
  ): ZIO[Blocking with context.Context, Throwable, Option[Action]] =
    ZIO.unit.flatMap(_ =>
      args match {
        case cmd :: next =>
          next match {
            case arg :: next =>
              context
                .fromWorkingDirectory(arg)
                .flatMap(pArg =>
                  ZIO.succeed(cmd match {
                    case "add"    => Some(Add(pArg))
                    case "remove" => Some(Remove(pArg))
                    case "backup" => Some(Backup(pArg))
                    case _        => None
                  })
                )
            case Nil => ZIO.succeed(None)
          }
        case Nil => ZIO.succeed(None)
      }
    )

}
