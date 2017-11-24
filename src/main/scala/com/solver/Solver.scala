package com.solver

import com.game.Game
import com.models.Card


object Solver {

  type IndexesOfSet = List[Int]

  def findSet(cardsOnTable: List[Card]): Option[IndexesOfSet] = {
      cardsOnTable
        .combinations(3)
        .find(list => Game.isASet(list(0), list(1), list(2)).isValid)
        .map(_.map(card => cardsOnTable.indexOf(card)))
  }
}
