package me.panavtec.tictactoe

import cats.free.{Free, Inject}
import cats.{Id, ~>}
import me.panavtec.tictactoe.Domain._

import scala.io.StdIn

sealed trait Console[A]
case object ReadLine extends Console[Coordinate]
case class PrintLine(line: String) extends Console[Unit]

object Console {
  val interpreter: Console ~> Id = new (Console ~> Id) {
    override def apply[A](fa: Console[A]): Id[A] = {
      fa match {
        case ReadLine => Coordinate.fromString(StdIn.readLine).get
        case PrintLine(line) => println(line)
      }
    }
  }
}

class ConsoleOps[F[_]](implicit I: Inject[Console, F]) {
  def readLine: Free[F, Coordinate] = Free.inject(ReadLine)

  def printLine(line: String): Free[F, Unit] = Free.inject(PrintLine(line))
}

object ConsoleOps {
  implicit def console[F[_]](implicit I: Inject[Console, F]): ConsoleOps[F] = new ConsoleOps[F]
}
