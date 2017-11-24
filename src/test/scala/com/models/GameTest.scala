package com.models

import org.scalatest.{FlatSpec, Matchers}

class GameTest extends FlatSpec with Matchers {

  "A new game" should "be created with a random set of cards" in {
    val player1 = Player()
    val player2 = Player()

    val game = Game.apply(List(player1, player2))
    game.players shouldBe List(player1, player2)
    game.gameState.cardsOnTable.size shouldBe 12

  }

}
