package me.panavtec.tictactoe

import shapeless.nat._
import shapeless.ops.zipper.First
import shapeless.{:+:, ::, CNil, Inl, Inr, _}

import scala.util.matching.Regex

object Domain {

  case class X()
  val X_ = Inl(X())
  case class O()
  val O_ = Inr(Inl(O()))
  case class E()
  val __ = Inr(Inr(Inl(E())))

  type Mark = X :+: O :+: CNil

  object Mark {
    def nextMark(m: Mark): Mark = m match {
      case X_ => O_
      case O_ => X_
    }
    def stringRepresentation(m: Mark): String = m match {
      case X_ => "X"
      case O_ => "O"
    }
  }

  type Bound = _1 :+: _2 :+: _3 :+: CNil
  val One = Inl(_1)
  val Two = Inr(Inl(_2))
  val Three = Inr(Inr(Inl(_3)))
  object Bound {
    def fromString(input: String): Option[Bound] = input match {
      case "1" => Some(One)
      case "2" => Some(Two)
      case "3" => Some(Three)
    }
  }
  type Coordinate = Bound :: Bound :: HNil
  object Coordinate {

    val boundRegex = "([1-3]{1})"
    val inputFormat: Regex = s"\\s*$boundRegex\\s*,\\s*$boundRegex\\s*".r

    def fromString(input: String): Option[Coordinate] = input match {
      case inputFormat(x, y) =>
        Bound.fromString(x).flatMap(b =>
          Bound.fromString(y).flatMap(b2 =>
            Some(b :: b2 :: HNil)
          ).orElse(None)
        ).orElse(None)
    }
  }

  type Board = Row :: Row :: Row :: HNil
  object Board {
    def emptyBoard: Board =
      (__ :: __ :: __ :: HNil) ::
        (__ :: __ :: __ :: HNil) ::
        (__ :: __ :: __ :: HNil) :: HNil

    def stringRepresentation(board: Board): String = {
      object StringRepresentation extends Poly2 {
        implicit val rCase: Case.Aux[String, Row, String] = at(_ + Row.stringRepresentation(_))
      }
      board.foldLeft("")(StringRepresentation)
    }

    def isBoardFull(board: Board): Boolean = board match {
      case r1 :: r2 :: r3 :: HNil => (r1.toList ++ r2.toList ++ r3.toList).takeWhile {
        case X_ => true
        case O_ => true
        case _ => false
      }.size == 9
    }

    def hasPlayerWon(board: Board): Boolean = {
      def isWinCombination(comb: Row) = comb match {
        case X_ :: X_ :: X_ :: HNil => true
        case O_ :: O_ :: O_ :: HNil => true
        case _ => false
      }

      def possibleWinCombinations: List[Row] = board match {
        case
          (tl :: t :: tr :: HNil) ::
            (l :: c :: r :: HNil) ::
            (bl :: b :: br :: HNil) :: HNil =>
          List(tl :: t :: tr :: HNil, l :: c :: r :: HNil, bl :: b :: br :: HNil,
            tl :: c :: br :: HNil, tr :: c :: bl :: HNil, tl :: l :: bl :: HNil,
            t :: c :: b :: HNil, tr :: r :: br :: HNil)
      }

      possibleWinCombinations.count(isWinCombination) >= 1
    }

    def playAt(board: Board, coordinate: Coordinate, newMark: Cell): Board = {
      def playAt(row: Row, bound: Bound) = (row, bound) match {
        case ((_ :: m :: r :: HNil), One) => newMark :: m :: r :: HNil
        case ((l :: _ :: r :: HNil), Two) => l :: newMark :: r :: HNil
        case ((l :: m :: _ :: HNil), Three) => l :: m :: newMark :: HNil
      }

      (board, coordinate) match {
        case ((top :: mid :: bot :: HNil), (x :: One :: HNil)) => playAt(top, x) :: mid :: bot :: HNil
        case ((top :: mid :: bot :: HNil), (x :: Two :: HNil)) => top :: playAt(mid, x) :: bot :: HNil
        case ((top :: mid :: bot :: HNil), (x :: Three :: HNil)) => top :: mid :: playAt(bot, x) :: HNil
      }
    }
  }
  type Row = Cell :: Cell :: Cell :: HNil
  object Row {
    object StringRepresentation extends Poly2 {
      implicit val rCase: Case.Aux[String, Row, String] = at(_ + Row.stringRepresentation(_))
      implicit val cCase: Case.Aux[String, Cell, String] = at(_ + Cell.stringRepresentation(_))
    }
    def stringRepresentation(row: Row): String = row.foldLeft("")(StringRepresentation) + "\n"
  }
  type Cell = X :+: O :+: E :+: CNil
  object Cell {
    def stringRepresentation(cell: Cell): String = cell match {
      case X_ => "X"
      case O_ => "O"
      case _ => " "
    }
  }

  def main(args: Array[String]): Unit = {
    val b: Board =
      (X_ :: X_ :: X_ :: HNil) ::
        (__ :: __ :: __ :: HNil) ::
        (__ :: __ :: __ :: HNil) :: HNil
    println(Board.stringRepresentation(b))
    println("Is board full: " + Board.isBoardFull(b))
    println("Has player won: " + Board.hasPlayerWon(b))
  }
}