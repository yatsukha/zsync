package zsync.config

import zio._
import zio.nio.core.file.Path
import java.{util => ju}
import java.text.SimpleDateFormat

sealed trait Mode

case class Recursive() extends Mode
case class Git()       extends Mode

case class Entry(path: Path, mode: Mode, date: ju.Date)

object Entry {

  def parse(line: String): ZIO[Any, Throwable, Entry] =
    ZIO.effect(line.trim.split(' ') match {
      case Array(p, m, d) =>
        Entry(
          Path(p),
          m.toLowerCase match {
            case "recursive" => Recursive()
            case "git"       => Git()
            case _ =>
              throw new RuntimeException("Invalid backup mode in list entry.")
          },
          new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss").parse(d)
        )
    })

}
