package com.models

import cats.data.Validated.{Invalid, Valid}
import com.cardops.CardOps

case class Game(players: List[Player], gameState: GameState) {

//  def makeMove(player: Player, card1: Card, card2: Card, card3: Card): Unit = {
//    CardOps.isASet(card1, card2, card3) match {
//      case Valid(_) => {
//        println("Valid move!")
//        this.copy(gameState = GameState(gameState.cardsOnTable.))
//      }
//      case Invalid(errList) => {
//        println(s"Invalid Move [${errList.toList.mkString(", ")}]")
//      }
//    }
//  }
}

case class GameState(cardsOnTable: List[Card], nextPlayerToMove: Player)

object Game {

  def apply(players: List[Player]): Game =

    players.headOption.fold(throw new RuntimeException("No players"))(firstPlayer =>
      Game(players, GameState(CardOps.generateRandomCards(12), firstPlayer)))


}
