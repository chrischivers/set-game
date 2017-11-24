package com.cardops

import cats.implicits._
import com.models.Card
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random


class CardOpsTest extends FlatSpec with Matchers {

  "A card set" should "be identified as a valid set when all values are different" in {
    val distinctCardSet = buildDistinctCardSet()
    CardOps.isASet(distinctCardSet(0), distinctCardSet(1), distinctCardSet(2)).isValid shouldBe true
  }

  it should "be identified as a valid set when all values are the same" in {
    val matchingCardSet = buildMatchingCardSet()
    val result = CardOps.isASet(matchingCardSet(0), matchingCardSet(1), matchingCardSet(2))
    result.isValid shouldBe true
  }

  it should "be identified as an invalid set when one feature has two of the same and one different" in {
    val distinctCardSet = buildDistinctCardSet()
    val almostDistinctCardSet = distinctCardSet.head.copy(shape = distinctCardSet.last.shape) :: distinctCardSet.tail
    val result = CardOps.isASet(almostDistinctCardSet(0), almostDistinctCardSet(1), almostDistinctCardSet(2))
    result.isValid shouldBe false
    result.toEither.left.get should have size 1
  }

  it should "be identified as an invalid set when all features have two of the same and one different" in {
    val distinctCardSet = buildDistinctCardSet()
    val invalidCardSet = distinctCardSet.head.copy(
      shape = distinctCardSet.last.shape,
      colour = distinctCardSet.last.colour,
      number = distinctCardSet.last.number,
      shading = distinctCardSet.last.shading) :: distinctCardSet.tail
    val result = CardOps.isASet(invalidCardSet(0), invalidCardSet(1), invalidCardSet(2))
    result.isValid shouldBe false
    result.toEither.left.get should have size 4
  }

  "Random generator" should "create n random cards" in {
    val n = Random.nextInt(10)
    val randomCards = CardOps.generateRandomCards(n)
    randomCards should have size n
  }

  "Remove cards from list" should "remove first card from list" in {
    val distinctCards = buildDistinctCardSet()
    val result = CardOps.removeCardsFromList(List(distinctCards.head), distinctCards)
    result shouldBe distinctCards.tail
  }

  it should "remove multiple cards from list" in {
    val distinctCards = buildDistinctCardSet()
    val cardsToRemove = Random.shuffle(distinctCards).take(3)
    val result = CardOps.removeCardsFromList(cardsToRemove, distinctCards)
    result should have size (distinctCards.size - cardsToRemove.size)
  }

  it should "remove only the one card from list where card in list multiple times" in {
    val distinctCards = buildDistinctCardSet()
    val cardsToRemove = Random.shuffle(distinctCards).take(3)
    val result = CardOps.removeCardsFromList(cardsToRemove, distinctCards ++ distinctCards)
    result should have size ((distinctCards ++ distinctCards).size - cardsToRemove.size)
  }

  def buildDistinctCardSet(): List[Card] = {
    import com.models.Feature._
    (allShapes, allColours, allNumbers, allShadings)
      .mapN { case (shape, colour, number, shading) => Card(shape, colour, number, shading) }
  }

  def buildMatchingCardSet(): List[Card] = {
    import com.models.Feature._
    val card = Card(allShapes.head, allColours.head, allNumbers.head, allShadings.head)
    List(card, card, card)
  }

}
