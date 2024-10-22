package io.github.edadma.typesetter

import java.awt.{Font => JFont, Graphics2D, RenderingHints, Toolkit}
import java.awt.font.TextLayout
import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage

import scala.compiletime.uninitialized

class Graphics2DTypesetter extends Typesetter:

  var page: BufferedImage = uninitialized
  var g: Graphics2D = uninitialized

  g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
  g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)

  private val frc = g.getFontRenderContext

  def init(): Unit =
    page = new BufferedImage(10, 10, BufferedImage.TYPE_INT_ARGB)
    g = page.createGraphics()

  def render(box: Box, width: Double, height: Double, xoffset: Double = 0, yoffset: Double = 0): Any =
    page = new BufferedImage(
      width.toInt,
      height.toInt,
      BufferedImage.TYPE_INT_ARGB,
    )
    g = page.createGraphics()
    g.setColor(java.awt.Color.WHITE)
    g.fillRect(0, 0, page.getWidth, page.getHeight)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    box.draw(this, xoffset, yoffset + box.ascent)
    page

  def getDPI: Double = Toolkit.getDefaultToolkit.getScreenResolution

  def setFont(font: Any): Unit = g.setFont(font.asInstanceOf[JFont])

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

  def getTextExtents(text: String): TextExtents =
    val layout = new TextLayout(text, currentFont.renderFont.asInstanceOf[JFont], frc)
    val bounds = layout.getBounds

    val ascent = -bounds.getY
//    val descent = layout.getDescent
    val width = bounds.getWidth
    val height = bounds.getHeight
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

//    val glyphs = font.createGlyphVector(frc, text)
//    val lb = glyphs.getLogicalBounds
//    val vb = glyphs.getVisualBounds
//
//    println(lb.getX)
//    println(lb.getWidth)
//    println(vb.getWidth)
//    TextExtents(lb.getX, vb.getY, vb.getWidth, vb.getHeight, lb.getWidth, 0)
