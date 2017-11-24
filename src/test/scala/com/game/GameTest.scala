package com.game

import com.TestUtils
import com.game.Game._
import com.models.Player
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class GameTest extends FlatSpec with Matchers with TestUtils {

  "A new game" should "be created with a random set of cards" in {
    val players = generatePlayers(4)
    val game = Game.newGame(players)

    game.players shouldBe players
    game.gameState.cardsOnTable.size shouldBe 12
  }

  "A valid move" should "award 1 point to player" in {
    val players = generatePlayers(4)
    val player1InitialState = players.head
    val cardsToSelect = getThreeValidDistinctCards()
    val startingGame = Game.newGame(players).copy(gameState = GameState(cardsToSelect ++ generateRandomCards(9)))

    val gameAfterMove = startingGame.makeMove(
      player1InitialState,
      card1Index = 0,
      card2Index = 1,
      card3Index = 2)
    val (player1, otherPlayers) = gameAfterMove.players.partition(_.id == player1InitialState.id)
    player1 should have size 1
    player1.head.score shouldBe 1
    otherPlayers should have size 3
    otherPlayers.forall(_.score == 0) shouldBe true
  }

  "An invalid move" should "deduct 1 point from player" in {
    val players = generatePlayers(4)
    val player1InitialState = players.head
    val cardsToSelect = getThreeInvalidCards()
    val startingGame = Game.newGame(players).copy(gameState = GameState(cardsToSelect ++ generateRandomCards(9)))

    val gameAfterMove = startingGame.makeMove(
      player1InitialState,
      card1Index = 0,
      card2Index = 1,
      card3Index = 2)
    val (player1, otherPlayers) = gameAfterMove.players.partition(_.id == player1InitialState.id)
    player1 should have size 1
    player1.head.score shouldBe -1
    otherPlayers should have size 3
    otherPlayers.forall(_.score == 0) shouldBe true
  }

  "A card set" should "be identified as a valid set when all values are different" in {
    val distinctCardSet = getThreeValidDistinctCards()
    isASet(distinctCardSet(0), distinctCardSet(1), distinctCardSet(2)).isValid shouldBe true
  }

  it should "be identified as a valid set when all values are the same" in {
    val matchingCardSet = getThreeValidMatchingCards()
    val result = isASet(matchingCardSet(0), matchingCardSet(1), matchingCardSet(2))
    result.isValid shouldBe true
  }

  it should "be identified as an invalid set when one feature has two of the same and one different" in {
    val distinctCardSet = getThreeValidDistinctCards()
    val almostDistinctCardSet = distinctCardSet.head.copy(shape = distinctCardSet.last.shape) :: distinctCardSet.tail
    val result = isASet(almostDistinctCardSet(0), almostDistinctCardSet(1), almostDistinctCardSet(2))
    result.isValid shouldBe false
    result.toEither.left.get should have size 1
  }

  it should "be identified as an invalid set when all features have two of the same and one different" in {
    val distinctCardSet = getThreeValidDistinctCards()
    val invalidCardSet = distinctCardSet.head.copy(
      shape = distinctCardSet.last.shape,
      colour = distinctCardSet.last.colour,
      number = distinctCardSet.last.number,
      shading = distinctCardSet.last.shading) :: distinctCardSet.tail
    val result = isASet(invalidCardSet(0), invalidCardSet(1), invalidCardSet(2))
    result.isValid shouldBe false
    result.toEither.left.get should have size 4
  }

  "Random card generator" should "create n random cards" in {
    val n = Random.nextInt(10)
    val randomCards = generateRandomCards(n)
    randomCards should have size n
  }

  "Change player score" should "increment the players score" in {
    val player1 = Player(id = "Player1")
    val player2 = Player(id = "Player2")

    val newPlayerList = changePlayerScore(player1, List(player1, player2))(_ + 1)
    newPlayerList.find(_.id == player1.id).get.score shouldBe 1
  }

  it should "decrement the players score" in {
    val player1 = Player(id = "Player1", score = 10)
    val player2 = Player(id = "Player2")

    val newPlayerList = changePlayerScore(player1, List(player1, player2))(_ - 1)
    newPlayerList.find(_.id == player1.id).get.score shouldBe 9
  }
}
