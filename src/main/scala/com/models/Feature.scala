package com.models

sealed trait Feature

sealed trait Shape extends Feature
case object Ovals extends Shape
case object Squiggles extends Shape
case object Diamonds extends Shape

sealed trait Colour extends Feature
case object Red extends Colour
case object Purple extends Colour
case object Green extends Colour

sealed trait Number extends Feature
case object One extends Number
case object Two extends Number
case object Three extends Number

sealed trait Shading extends Feature
case object Solid extends Shading
case object Striped extends Shading
case object Outlined extends Shading

case object Feature {
  val allColours = List(Red, Purple, Green)
  val allShapes = List(Ovals, Squiggles, Diamonds)
  val allNumbers = List(One, Two, Three)
  val allShadings = List(Solid, Striped, Outlined)
}