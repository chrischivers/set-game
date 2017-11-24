package com.solver

import com.TestUtils
import com.game.Game
import org.scalatest.{FlatSpec, Matchers}

class SolverTest extends FlatSpec with Matchers with TestUtils {

  "Solver" should "identify three set cards from cards on the table" in {
    val validCards = getThreeValidDistinctCards()
    val cards = validCards ++ Game.generateRandomCards(9)
    val result = Solver.findSet(cards).get
    result should have size 3
    result shouldBe List(0, 1, 2)
  }

  it should "return none when no sets found" in {
    val invalidCards = getThreeInvalidCards()
    val result = Solver.findSet(invalidCards)
    result should not be defined
  }
}
