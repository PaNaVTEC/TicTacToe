package me.panavtec.tictactoe

import cats.data.Coproduct
import cats.free.Free
import cats.{Id, ~>}
import me.panavtec.tictactoe.Domain.{Board, Coordinate, Mark}

object GameEngine {

  type PlayerInput = (Coordinate, Mark)
  type GameState = (Mark, Board)
  type TicTacToeApp[A] = Coproduct[Console, Game, A]
  val interpreter: TicTacToeApp ~> Id = Console.interpreter or Game.interpreter

  val I: ConsoleOps[TicTacToeApp] = implicitly[ConsoleOps[TicTacToeApp]]
  val D: GameOps[TicTacToeApp] = implicitly[GameOps[TicTacToeApp]]

  import D._
  import I._

  def init: GameState = startTicTacToe.flatMap(initialGameState =>
    iterateUntil(Game.hasFinished)(initialGameState, turn)
  ).foldMap(interpreter)

  def startTicTacToe: Free[TicTacToeApp, GameState] = {
    for {
      _ <- printLine("== Game Starts ==")
      gameState <- startGame
    } yield gameState
  }

  def turn(gameState: GameState): Free[TicTacToeApp, GameState] = {
    val (player: Mark, board: Board) = gameState
    for {
      _ <- printLine(s"Turn Of Player ${Mark.stringRepresentation(player)}. Input next move [X,Y]:")
      coordinate <- readLine
      newGameState <- playNext(board, (coordinate, player))
      _ <- printLine(Board.stringRepresentation(newGameState._2))
    } yield newGameState
  }

  def iterateUntil[A[_], S](pred: S => Boolean)(initialState: S, next: S => Free[A, S]): Free[A, S] = {
    next(initialState).flatMap(nextState =>
      if (pred(nextState)) Free.pure(nextState)
      else iterateUntil(pred)(nextState, next)
    )
  }
}