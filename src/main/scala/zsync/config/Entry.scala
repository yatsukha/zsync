package zsync.config

import zio._
import zio.nio.core.file.Path
import java.{util => ju}
import java.text.SimpleDateFormat

sealed trait Mode

case class Recursive() extends Mode
case class Git()       extends Mode

case class Entry(path: Path, mode: Mode)

object Entry {

  def parse(line: String): ZIO[Any, Throwable, Entry] =
    ZIO.effect(line.trim.split(' ') match {
      case Array(p, m) =>
        Entry(
          Path(p),
          m.toLowerCase match {
            case "recursive" => Recursive()
            case "git"       => Git()
            case _ =>
              throw new RuntimeException("Invalid backup mode in list entry.")
          }
        )
      case _ =>
        throw new RuntimeException(
          "Invalid line format in config file. Did you edit it manually?"
        )
    })

}
