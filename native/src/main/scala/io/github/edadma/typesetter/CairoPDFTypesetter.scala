package io.github.edadma.typesetter

import io.github.edadma.freetype.initFreeType
import io.github.edadma.libcairo.{
  Context,
  FontFace,
  FontSlant,
  FontWeight,
  Format,
  Surface,
  TextExtents,
  fontFaceCreateForFTFace,
  imageSurfaceCreate,
  pdfSurfaceCreate,
}
import scala.compiletime.uninitialized

class CairoPDFTypesetter extends Typesetter:

  private var surface: Surface = uninitialized
  private var ctx: Context     = uninitialized

  def initTarget(): Unit = ()

  def createPageTarget(path: String, width: Double, height: Double): Any =
    if surface eq null then
      surface = pdfSurfaceCreate(path, width, height)
      ctx = surface.create

  def ejectPageTarget(): Unit = ctx.showPage()

  def getDPI: Double = 72

  def setFont(font: Any): Unit = ctx.setFontFace(font.asInstanceOf[FontFace])

  def setColor(color: Color): Unit =
    g.setColor(new java.awt.Color(color.redInt, color.greenInt, color.blueInt, color.alphaInt))

  def drawString(text: String, x: Double, y: Double): Unit = g.drawString(text, x.toFloat, y.toFloat)
  def drawLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit =
    g.drawLine(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
  def drawRect(x: Double, y: Double, width: Double, height: Double): Unit =
    g.drawRect(x.toInt, y.toInt, width.toInt, height.toInt)
  def fillRect(x: Double, y: Double, width: Double, height: Double): Unit =
    g.fillRect(x.toInt, y.toInt, width.toInt, height.toInt)

  def loadFont(path: String): JFont = java.awt.Font.createFont(java.awt.Font.TRUETYPE_FONT, new java.io.File(path))

  def getTextExtents(text: String, font: Any): TextExtents =
    val layout = new TextLayout(text, font.asInstanceOf[JFont], frc)
    val bounds = layout.getBounds

    val ascent = -bounds.getY
//    val descent = layout.getDescent
    val width   = bounds.getWidth
    val height  = bounds.getHeight
    val advance = layout.getAdvance

    TextExtents(
      xBearing = bounds.getX,
      yBearing = -ascent, // In Graphics2D, the ascent is negative yBearing (above the baseline)
      width = width,
      height = height,
      xAdvance = advance,
      yAdvance = 0, // In horizontal typesetting, yAdvance is 0
    )

  def makeFont(font: Any, size: Double): Any = font.asInstanceOf[JFont].deriveFont(size.toFloat)

  def charWidth(font: Any, c: Char): Double =
    setFont(font)
    g.getFontMetrics.charWidth(c)

  def loadImage(path: String): (Any, Int, Int) =
    val image = ImageIO.read(new File(path))

    (image, image.getWidth, image.getHeight)

  def drawImage(image: Any, x: Double, y: Double): Unit =
    g.drawImage(image.asInstanceOf[BufferedImage], x.toInt, y.toInt, null)
