package io.github.edadma.typesetter

class HSpaceBox(val width: Double) extends SpaceBox:

  val descent: Double = 0
  val xAdvance: Double = width

  override def toString: String = s"HSpaceBox(width=$width)"
