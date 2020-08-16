package zsync

package object util {
  import zio._

  def using[A: Tag, R](a: Has[A])(af: A => R): R = af(a.get)

  def nonNull[A](a: A): A = a match {
    case null => throw new NullPointerException()
    case _    => a
  }

}
