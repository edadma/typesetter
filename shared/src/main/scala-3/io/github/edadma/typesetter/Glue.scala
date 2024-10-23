package io.github.edadma.typesetter

class Glue(
    val naturalSize: Double,
    val stretch: Double = 0,
    val shrink: Double = 0,
    val order: Int = 0,
    val nobreak: Boolean = false,
) extends SpaceBox:

  val descent: Double = naturalSize
  val width: Double = naturalSize // Initially, it's the natural width
  val xAdvance: Double = naturalSize // Same as width initially

  def -(amount: Double): Glue = Glue(naturalSize - amount, stretch, shrink, order, nobreak)
  def +(amount: Double): Glue = Glue(naturalSize + amount, stretch, shrink, order, nobreak)
  def *(amount: Double): Glue = Glue(naturalSize * amount, stretch, shrink, order, nobreak)
  def noBreak: Glue = Glue(naturalSize, stretch, shrink, order, true)

  override def toString: String =
    s"$Glue(naturalSize=$naturalSize, stretch=$stretch, shrink=$shrink, order=$order, nobreak=$nobreak)"
end Glue

val FilGlue = Glue(0, 1, 0, 1)

val FillGlue = Glue(0, 1, 0, 2)

val ZeroGlue = Glue(0, 0, 0, 0)

val InfGlue = Glue(0, 1, 1, 1)
