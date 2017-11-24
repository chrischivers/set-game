package com

import com.models.Feature._
import com.models.{Card, Player}

trait TestUtils {

  def getThreeValidDistinctCards(): List[Card] = {
    List(
      Card(allShapes(0), allColours(0), allNumbers(0), allShadings(0)),
      Card(allShapes(1), allColours(1), allNumbers(1), allShadings(1)),
      Card(allShapes(2), allColours(2), allNumbers(2), allShadings(2))
    )
  }

  def getThreeValidMatchingCards(): List[Card] = {
    val card = Card(allShapes.head, allColours.head, allNumbers.head, allShadings.head)
    List(card, card, card)
  }

  def getThreeInvalidCards(): List[Card] = {
    getThreeValidDistinctCards().dropRight(1) :+ getThreeValidDistinctCards().head
  }

  def generatePlayers(numberPlayers: Int): List[Player] = (0 until numberPlayers).map(_ => Player()).toList



}
