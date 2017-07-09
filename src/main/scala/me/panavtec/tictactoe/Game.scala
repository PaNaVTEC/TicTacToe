package me.panavtec.tictactoe

import cats.free.{Free, Inject}
import cats.{Id, ~>}
import me.panavtec.tictactoe.Domain.{Board, Mark, O_, X_}
import me.panavtec.tictactoe.GameEngine.{GameState, PlayerInput}

sealed trait Game[A]
case object Start extends Game[GameState]
case class PlayAt(board: Board, playerInput: PlayerInput) extends Game[GameState]

object Game {
  val interpreter: Game ~> Id = new (Game ~> Id) {
    override def apply[A](fa: Game[A]): Id[A] = {
      fa match {
        case Start => (X_, Board.emptyBoard)
        case PlayAt(board, (coordinate, X_)) => (O_, Board.playAt(board, coordinate, X_))
        case PlayAt(board, (coordinate, O_)) => (X_, Board.playAt(board, coordinate, O_))
      }
    }
  }

  def hasFinished(gs: GameState): Boolean = gs match {
    case (_ , b) => Board.hasPlayerWon(b) || Board.isBoardFull(b)
  }
}

class GameOps[F[_]](implicit I: Inject[Game, F]) {
  def startGame: Free[F, GameState] = Free.inject(Start)

  def playNext(board: Board, playerInput: PlayerInput): Free[F, GameState] = Free.inject(PlayAt(board, playerInput))
}

object GameOps {
  implicit def turn[F[_]](implicit I: Inject[Game, F]): GameOps[F] = new GameOps[F]
}

