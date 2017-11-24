package com.game

import cats.data.Validated.{Invalid, Valid}
import com.models._

case class Game(players: List[Player], gameState: GameState) {

  import Game._

  def makeMove(player: Player, card1Index: Int, card2Index: Int, card3Index: Int): Game = {
    val card1 = gameState.cardsOnTable(card1Index)
    val card2 = gameState.cardsOnTable(card2Index)
    val card3 = gameState.cardsOnTable(card3Index)
    isASet(card1, card2, card3) match {

      case Valid(_) =>
        println(s"Valid move by player ${player.id}!")
        this.copy(
          players = changePlayerScore(player, this.players)(_ + 1),
          gameState = GameState(
            gameState.cardsOnTable
              .patch(card1Index, generateRandomCards(1), 1)
              .patch(card2Index, generateRandomCards(1), 1)
              .patch(card3Index, generateRandomCards(1), 1)
        ))


      case Invalid(errList) =>
        println(s"Invalid Move by player ${player.id} [${errList.toList.mkString(", ")}]")
        this.copy(players = changePlayerScore(player, this.players)(_ - 1))
    }
  }
}

object Game {

  import Feature._
  import cats.data.Validated._
  import cats.data._
  import cats.implicits._

  import scala.util.Random


  def newGame(players: List[Player]): Game =
    Game(players, GameState(generateRandomCards(12)))

  type ValidationResult[A] = ValidatedNel[String, A]

  private def invalidMessage(feature: String) = s"$feature invalidates the set "

  def isASet(card1: Card, card2: Card, card3: Card): ValidationResult[Unit] = {

    def validateFeature[T <: Feature](featureName: String, list: List[T]): ValidationResult[Unit] =
      if (list.distinct.size != 2) ().valid
      else invalidMessage(featureName).invalidNel

    val cardList = List(card1, card2, card3)
    (validateFeature("Shape", cardList.map(_.shape)),
      validateFeature("Colour", cardList.map(_.colour)),
      validateFeature("Number", cardList.map(_.number)),
      validateFeature("Shading", cardList.map(_.shading))).mapN { case (_, _, _, _) => () }
  }

  def generateRandomCards(n: Int): List[Card] = (0 until n).map(_ => generateRandomCard).toList

  private def generateRandomCard: Card =
    Card(
      Random.shuffle(allShapes).head,
      Random.shuffle(allColours).head,
      Random.shuffle(allNumbers).head,
      Random.shuffle(allShadings).head)

  def changePlayerScore(player: Player, playerList: List[Player])(f: Int => Int): List[Player] = {
    playerList.map(p => if(p == player) p.copy(score = f(p.score)) else p)
  }
}
