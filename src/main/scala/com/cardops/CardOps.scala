package com.cardops

import com.models.{Card, Feature}
import cats.data._
import cats.data.Validated._
import cats.implicits._
import Feature._
import scala.util.Random

object CardOps {

  type ValidationResult[A] = ValidatedNel[String, A]

  private def invalidMessage(feature: String) = s"$feature invalidates the set "

  def isASet(card1: Card, card2: Card, card3: Card): ValidationResult[Unit] = {

    def validateFeature[T <: Feature](featureName: String, list: List[T]): ValidationResult[Unit] =
      if (list.distinct.size == 1 || list.distinct.size == 3) ().valid
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

  def removeCardsFromList(cardsToRemove: List[Card], cardsToRemoveFrom: List[Card]) = {
    cardsToRemove.foldLeft(cardsToRemoveFrom){(listAcc, card) => removeCardFromList(card, listAcc)}
  }

  private def removeCardFromList(toRemove: Card, toRemoveFrom: List[Card] ) : List[Card] = toRemoveFrom.head match {
    case matching if matching == toRemove => toRemoveFrom.tail
    case other => List(other) ++ removeCardFromList(toRemove, toRemoveFrom.tail)
  }
}
