package typesetter

import java.awt.{AlphaComposite, Graphics2D, Font => JFont, Color, Shape, BasicStroke, Stroke}
import java.awt.image.BufferedImage
import java.awt.font.{GlyphVector, TextLayout}
import java.awt.geom._
import java.awt.Color._

import collection.mutable.ListBuffer


trait Item
{
	def kind: String
	
	def isHorizontal: Boolean
	
	def isSkip = isInstanceOf[Skip]
	
	def asSkip = asInstanceOf[Skip]
	
	def isSpace = isSkip || isInstanceOf[SpaceBox]
	
	def isBox = isInstanceOf[Box]
	
	def asBox = asInstanceOf[Box]
	
	def isBoxer = isInstanceOf[Boxer]
	
	def asBoxer = asInstanceOf[Boxer]
	
	def isGraphics = isInstanceOf[Rule] || isInstanceOf[GraphicsBox]
	
	def isMeasurable = isInstanceOf[Measureable]
	
	def asMeasurable = asInstanceOf[Measureable]
	
	def show = println( this )
	
	override def toString = "<" + kind + ">"
}

trait Measureable extends Item		// "Measureable" is deliberately misspelled
{
	def width: Double

	def height = ascent + descent

	def ascent: Double

	def descent: Double
	
	def measure = (width, ascent, descent)
	
	override def toString = "<" + kind + ": width = " + width + ", height = " + height + ", ascent = " + ascent + ">"
}

trait Box extends Measureable
{	
	def draw( g: Graphics2D, x: Double, y: Double )

	def box( x: Double, y: Double ) = new Rectangle2D.Double( x, y, if (width == 0) 3 else width abs, if (height == 0) 3 else height abs )
	
	def drawBox( g: Graphics2D, x: Double, y: Double, c: Color )
	{
		g.setColor( c )
		g.draw( box(x, y) )
	}

	def offset( amount: Double ) {}
	
	def set {}

	def reset {}
}

trait GraphicsBox extends Box

trait Boxer extends Measureable
{
	def box( m: Measureable ): Box
}

abstract class Rule( val isHorizontal: Boolean, val c: Color, val a: AlphaComposite ) extends Boxer
{
	def kind = if (isHorizontal) "VRule" else "HRule"
}

class HRule( w: Option[Double], val ascent: Double, val descent: Double, c: Color, a: AlphaComposite ) extends Rule( false, c, a )
{
	def width = if (w == None) 0 else w.get
	
	override def box( m: Measureable ) = new RuleBox( if (w == None) m.width else w.get, ascent, descent, c, a )
}

object HRule
{
	def apply( t: Typesetter, thickness: Double ) = new HRule( None, thickness, 0, t.color, t.alpha )
	
	def apply( t: Typesetter, ascent: Double, descent: Double ) = new HRule( None, ascent, descent, t.color, t.alpha )
}

class VRule( val width: Double, a: Option[Double], d: Option[Double], c: Color, alpha: AlphaComposite ) extends Rule( true, c, alpha )
{
	def ascent = if (a == None) 0 else a.get
	
	def descent = if (d == None) 0 else d.get

	def box( m: Measureable ) = new RuleBox( width, if (a == None) m.ascent else a.get, if (d == None) m.descent else d.get, c, alpha )
}

object VRule
{
	def apply( t: Typesetter, thickness: Double ) = new VRule( thickness, None, None, t.color, t.alpha )
}

class SpaceBox( val isHorizontal: Boolean, val width: Double, val ascent: Double, val descent: Double, leader: Measureable ) extends Box
{
	private val leaderBox =
		leader match
		{
			case b: Boxer => b.box( this )
			case b: Box => b
			case null => null
		}

	private var _offset: Option[Double] = None
	
	def kind = if (isHorizontal) "HSpace" else "VSpace"

	override def offset( amount: Double )
	{
		require( _offset == None )
		
		_offset = Some( amount )
	}
	
	def draw( g: Graphics2D, x: Double, y: Double )
	{
		if (leaderBox ne null)
		{
			leader match
			{
				case _: Boxer => leaderBox.draw( g, x, y )
				case _: Box =>
					if (_offset == None) sys.error( "SpaceBox: offset expected" )
					
					if (isHorizontal)
					{
					val a = _offset.get/leaderBox.width
					val align_offset = leaderBox.width - (a - a.toInt)*leaderBox.width
					var curx = x + align_offset
					
						for (i <- 0 until ((width - align_offset)/leaderBox.width).toInt)
						{
							leaderBox.draw( g, curx, y )
							curx += leaderBox.width
						}
					}
			}
		}
//		drawBox( g, x, y, RED )
	}
}

object HKern extends (Double => SpaceBox)
{
	def apply( width: Double ) = new SpaceBox( true, width, 3, 0, null )
}

object VKern extends (Double => SpaceBox)
{
	def apply( height: Double ) = new SpaceBox( true, 3, height, 0, null )
}

object HSpace extends ((Double, Measureable) => SpaceBox)
{
	def apply( width: Double, leader: Measureable ) = new SpaceBox( true, width, 3, 0, leader )
}

object VSpace extends ((Double, Measureable) => SpaceBox)
{
	def apply( height: Double, leader: Measureable ) = new SpaceBox( false, 3, height, 0, leader )
}

object Space extends ((Double, Double) => SpaceBox)
{
	def apply( width: Double, height: Double ) = new SpaceBox( true, width, height, 0, null )
}

case class Glue( natural: Double, plus: Double, order: Int, minus: Double )
{
	def add( a: Double ) = Glue( natural + a, plus, order, minus )
	
	def mul( a: Double ) = Glue( a*natural, plus, order, minus )
	
	def sub( s: Double ) = Glue( natural - s, plus, order, minus )
	
	def fix = Glue( natural, 0, order, 0 )
}

object Glue
{
	def apply( n: Double ): Glue = Glue( n, 0, 0, 0 )
}

object ZGlue extends Glue( 0, 0, 0, 0 )

object FGlue extends Glue( 0, 1, 1, 0 )

trait Skip extends Measureable
{
	def kind =
		{
		val fill = Array( "", "fil", "fill", "filll" )
		
			(if (isHorizontal) "Hor. Glue" else "Ver. Glue") + " plus: " + plus + fill(order) + " minus: " + minus + fill(order)
		}
	
	val plus: Double
	
	val order: Int
	
	val minus: Double
	
	require( order >= 0, "glue order must be nonnegative" )
	require( order <= 3, "glue order cannot exceed 3" )

	val descent = 0D
	
	def natural: Double
	
	def leader( m: Measureable )
	
	def space( amount: Double ): SpaceBox
	
	def naturalSpace = space( natural )
}

abstract class Leader extends Skip
{
	protected var _leader: Measureable = null
	
	def leader( m: Measureable )
	{
		require( m.isInstanceOf[Box] || m.isInstanceOf[Boxer], "a leader should comprise a Box or Boxer object" )
		
		if (_leader ne null)
			sys.error( "leader has already been set" )
			
		_leader = m
	}
	
	def space( amount: Double ) = (if (isHorizontal) HSpace else VSpace)( amount, _leader )
}

class HSkip( val width: Double, val plus: Double, val order: Int, val minus: Double ) extends Leader
{
	def this( g: Glue ) = this( g.natural, g.plus, g.order, g.minus )
	
	val ascent = 0D
	
	val isHorizontal = true
	
	def natural = width
}

object HSkip
{
	def apply( width: Double ) = new HSkip( Glue(width) )
}

class VSkip( val ascent: Double, val plus: Double, val order: Int, val minus: Double ) extends Leader with Skip
{
	def this( g: Glue ) = this( g.natural, g.plus, g.order, g.minus )
	
	val width = 0D
	
	val isHorizontal = false
	
	def natural = ascent
}

object VSkip
{
	def apply( ascent: Double ) = new VSkip( Glue(ascent) )
}

abstract class FillSkip extends Leader with Skip
{
	val width = 0D

	val ascent = 0D
	
	val natural = 0D
}

abstract class Fil( val order: Int = 1 ) extends FillSkip
{
	val plus = 1D
	
	val minus = 0D
}

class HFil extends Fil
{
	val isHorizontal = true
}

class VFil extends Fil
{
	val isHorizontal = false
}

abstract class Fill( val order: Int = 2 ) extends FillSkip
{
	val plus = 1D
	
	val minus = 0D
}

class HFill extends Fill
{
	val isHorizontal = true
}

class VFill extends Fill
{
	val isHorizontal = false
}

abstract class Filneg( val order: Int = 1 ) extends FillSkip
{
	val plus = -1D
	
	val minus = 0D
}

class HFilneg extends Filneg
{
	val isHorizontal = true
}

class VFilneg extends Filneg
{
	val isHorizontal = false
}

abstract class Fss( val order: Int = 1 ) extends FillSkip
{
	val plus = 1D
	
	val minus = 1D
}

class Hss extends Fss
{
	val isHorizontal = true
}

class Vss extends Fss
{
	val isHorizontal = false
}

class Shift( b: Box, shift: Double ) extends Box
{
	val kind = "Shifted " + b.kind
	
	def isHorizontal = b.isHorizontal
	
	def width = b.width
	
	def ascent =
		if (shift >= -b.ascent)
			b.ascent + shift
		else
			0
	
	def descent =
		if (shift >= b.descent)
			0
		else
			b.descent - shift
	
	def draw( g: Graphics2D, x: Double, y: Double ) = b.draw( g, x, if (shift < -b.height) y - shift - b.height else y )
	
	override def box( x: Double, y: Double ) = b.box( x, if (shift < -b.height) y - shift - b.height else y )

	override def set = b.set
}

class Move( b: Box, move: Double ) extends Box
{
	val kind = "Moved " + b.kind
	
	def isHorizontal = b.isHorizontal
	
	def width = b.width + move
	
	def ascent = b.ascent
	
	def descent = b.descent
	
	def draw( g: Graphics2D, x: Double, y: Double ) = b.draw( g, x + move, y )
	
	override def box( x: Double, y: Double ) = b.box( x + move, y )

	override def set = b.set
}

trait Reference
object TopLeft extends Reference
object MiddleLeft extends Reference
object BottomLeft extends Reference
object TopCenter extends Reference
object MiddleCenter extends Reference
object BottomCenter extends Reference
object TopRight extends Reference
object MiddleRight extends Reference
object BottomRight extends Reference
object OriginReference extends Reference

class Placement( b: Box, val xpos: Double, val ypos: Double, val ref: Reference ) extends Box
{
	val kind = "Placement " + b.kind
	
	def isHorizontal = b.isHorizontal
	
	def width = b.width
	
	def ascent = b.ascent
	
	def descent = b.descent
	
	val (minx, miny) =
		if (b.isInstanceOf[ShapeBox])
			(b.asInstanceOf[ShapeBox].bounds.getX, b.asInstanceOf[ShapeBox].bounds.getY)
		else
			(0D, 0D)
	
	val (startx, starty) =
		if (b.isInstanceOf[ShapeBox] && b.asInstanceOf[ShapeBox].shape.isInstanceOf[Path2D.Double])
		{
		val it = b.asInstanceOf[ShapeBox].shape.asInstanceOf[Path2D.Double].getPathIterator( null )
		val coords = new Array[Double]( 6 )
		
			if (it.currentSegment( coords ) != PathIterator.SEG_MOVETO)
				sys.error( "initial path operation was not a 'moveTo' " + b )
				
			(coords(0), coords(1))
		}
		else
			(0D, 0D)
	
	def offsets =
		ref match
		{
		case TopLeft => (xpos, ypos)
		case MiddleLeft => (xpos, ypos - height/2)
		case BottomLeft => (xpos, ypos - height)
		case TopCenter => (xpos - width/2, ypos)
		case MiddleCenter => (xpos - width/2, ypos - height/2)
		case BottomCenter => (xpos - width/2, ypos - height)
		case TopRight => (xpos - width, ypos)
		case MiddleRight => (xpos - width, ypos - height/2)
		case BottomRight => (xpos - width, ypos - height)
		case OriginReference => (xpos + minx, ypos + miny)
		}

	def draw( g: Graphics2D, x: Double, y: Double )
	{
	val (xoff, yoff) = offsets
	
		b.draw( g, x + xoff, y + yoff )
	}
	
	override def box( x: Double, y: Double ) =
	{
	val (xoff, yoff) = offsets
	
		b.box( x + xoff, y + yoff )
	}

	override def set = b.set
}

class DrawBox( size: Option[(Double, Double)], list: List[Placement] ) extends GraphicsBox
{
	val kind = "DrawBox"
	
	private var w: Double = _
	private var h: Double = _
	private var prew: Double = _
	private var preh: Double = _
	
	set
	
	def isHorizontal = false
	
	def width = w
	
	def ascent = h
	
	def descent = 0D
	
	override def height = h
	
	def draw( g: Graphics2D, x: Double, y: Double )
	{
		list foreach (_.draw( g, x - prew, y - preh ))
	}

	override def set
	{
		if (size == None)
		{
			w = 0
			h = 0
			
			if (list.isEmpty)
			{
				prew = 0
				preh = 0
			}
			else
			{
				prew = Double.MaxValue
				preh = Double.MaxValue
				
				for (p <- list)
				{
				val (xo, yo) = p.offsets
				
					prew = prew min xo			
					preh = preh min yo
					w = w max (xo + p.width)			
					h = h max (yo + p.height)
				}
				
				w -= prew
				h -= preh
			}
		}
		else
		{
			w = size.get._1
			h = size.get._2
		}
	}
}

abstract class Background( b: Box, top: Double, right: Double, bottom: Double, left: Double ) extends Box
{
	val kind = "Background " + b.kind
	
	def isHorizontal = b.isHorizontal
	
	def width = b.width + left + right
	
	def ascent = b.ascent + top
	
	def descent = b.descent + bottom
	
	def draw( g: Graphics2D, x: Double, y: Double )
	{
		drawBackground( g, x, y )
		b.draw( g, x + left, y + top )
	}
	
	def drawBackground( g: Graphics2D, x: Double, y: Double )
}

class TranslucentBox( b: Box, margin: Double, opacity: Double ) extends Background( b, margin, margin, margin, margin )
{
	require( opacity >= 0 && opacity <= 1 )
	
	private val comp = AlphaComposite.getInstance( AlphaComposite.SRC_OVER, opacity.toFloat )
	private val rect = new Rectangle2D.Double( 0, 0, width, ascent + descent )
	
	def drawBackground( g: Graphics2D, x: Double, y: Double )
	{
	val t = g.getTransform
	
		g.translate( x, y )
		g setColor BLACK
		g setComposite comp
		g fill rect
		g.setTransform( t )
	}
}

abstract class ListBox extends Box
{
	protected var valid = false
	
	val list: List[Item]
	
	override def reset
	{
		valid = false
		
		for (b <- list)
			if (b.isBox)
				b.asBox.reset
				
		set
	}

	trait GlueSet
	{
		val size: Double
	}
	
	case class Ideal( size: Double ) extends GlueSet
	case class Underfull( size: Double, underflow: Double ) extends GlueSet
	case class Overfull( size: Double, overflow: Double ) extends GlueSet
	
	protected def glue( size: Size, horizontal: Boolean, array: Array[Box] ) =
	{
	val natural = if (horizontal) Util.width( list ) else Util.height( list )
	val tosize =
		size match
		{
		case To( s ) => s
		case Spread( s ) => natural + s
		case _ => natural
		}
	var stretch = 0D
	var shrink = 0D

		if (size != Natural)
		{
		var maxorder = -1
			
			for (b <- list)
				if (b.isSkip)
					maxorder = maxorder max b.asInstanceOf[Skip].order

		val fills = new ListBuffer[(Skip, Int)]
		
			if (maxorder > -1)
			{
			var index = 0
			
				for (b <- list)
				{
					if (b.isSkip)
						if (b.asSkip.order == maxorder)
						{
							fills += ((b.asSkip, index))
							stretch += b.asSkip.plus
							shrink += b.asSkip.minus
						}
						else
							array(index) = b.asSkip.naturalSpace
					
					index += 1
				}
			}
			
		val divisor = if (natural < tosize) stretch else shrink
		val unit = (tosize - natural)/divisor
			
			if (!fills.isEmpty && divisor > 0)
			{
				for ((f, i) <- fills)
					array(i) = f.space( f.natural + (if (natural < tosize) f.plus else f.minus)*unit )
			}
			else
			{
			var index = 0
	
				for (b <- list)
				{
					if (b.isSkip)
						array(index) = b.asSkip.naturalSpace
					
					index += 1
				}
			}
		}
		else
		{
		var index = 0

			for (b <- list)
			{
				if (b.isSkip)
					array(index) = b.asSkip.naturalSpace
				
				index += 1
			}
		}
		
		if (tosize < natural && shrink < natural - tosize)
			Overfull( tosize, natural - tosize - shrink )
		else if (tosize > natural && stretch < tosize - natural)
			Underfull( tosize, tosize - natural - stretch )
		else
			Ideal( tosize )
	}
}

trait Size
case class To( size: Double ) extends Size
case class Spread( amount: Double ) extends Size
case object Natural extends Size

class HBox( val list: List[Item], size: Size = Natural ) extends ListBox
{
	private val array =
		{
		val ret = new Array[Box]( list.length )
		var index = 0
			
			for (b <- list)
			{
				if (b.isBox)
					ret(index) = b.asBox
				
				index += 1
			}
			
			ret
		}
	private var w = 0D
	private var d = 0D
	private var a = 0D
	private var h = 0D

	val kind = "HBox"
	
	val isHorizontal = false
	
	set
	
	def draw( g: Graphics2D, x: Double, y: Double )
	{
	var curx = x
	
		for (i <- array)
		{
			i.draw( g, curx, y + a - i.ascent )
			curx += i.width
		}
	}
	
	override def set =
		if (!valid)
		{
			w = 0
			d = 0
			a = 0
			
			for (b <- list)
			{
				if (b.isMeasurable)
				{
					a = a max b.asMeasurable.ascent
					d = d max b.asMeasurable.descent
				}
			}
	
			var index = 0
			
			for (b <- list)
			{
				if (b.isBoxer)
					array(index) = b.asBoxer.box( this )
					
				index += 1
			}
			
			w = glue( size, true, array ) match
			{
				case Ideal( s ) => s
				case Underfull( s, u ) =>
//					println( "underfull hbox: " + u )
					s
				case Overfull( s, o ) =>
//					println( "overfull hbox: " + o )
					s
			}
			
			var offset = 0D
			
			for (b <- array)
			{
				b.offset( offset )
				offset += b.width
			}
			
			valid = true
	}

	def width = w

	def descent = d

	def ascent = a
	
	override def show = println( this + array.mkString("( \n\t", ",\n\t", " )") )
}

object HBox
{
	def apply( boxes: List[Item] ) = new HBox( boxes )
}

class VBox( val list: List[Item], size: Size, baseline: Int = -1 ) extends ListBox
{
	private var w = 0D
	private var h = 0D
	private var a = 0D
	private var d = 0D
	private val array =
		{
		val ret = new Array[Box]( list.length )
		var index = 0
			
			for (b <- list)
			{
				if (b.isBox)
					ret(index) = b.asBox
				
				index += 1
			}
			
			ret
		}
	
	val kind = "VBox"
	
	val isHorizontal = false
	
	set
	
	def draw( g: Graphics2D, x: Double, y: Double )
	{
	var cury = y
	
		for (b <- array)
		{
//			println( b )
			if (b.isBox)
				b.asBox.draw( g, x, cury )

			if (b.isMeasurable)
				cury += b.asMeasurable.height
		}
	}

	override def set =
		if (!valid)
		{
		var count = baseline
		var last: Item = null
		
			w = 0
			h = 0
			a = 0
			d = 0
			
			for (b <- list)
			{
				last = b
					
				if (b.isMeasurable)
				{
					if (count == 0)
						a = h + b.asMeasurable.ascent
					if (count >= 0)
						count -= 1
					
					w = w max b.asMeasurable.width
				}
			}
	
			var index = 0
			
			for (b <- list)
			{
				if (b.isBoxer)
					array(index) = b.asBoxer.box( this )
					
				index += 1
			}
	
			h = glue( size, false, array ) match
			{
				case Ideal( s ) => s
				case Underfull( s, u ) =>
//					println( "underfull vbox: " + u )
					s
				case Overfull( s, o ) =>
//					println( "overfull vbox: " + o )
					s
			}
	
			if (baseline < 0)
				if (last ne null)
					a = h - last.asMeasurable.descent
				else
					a = h
			
			valid = true
		}
	
	def width = w

	override def height = h

	def ascent = a

	def descent = h - a
	
	override def show = println( this + array.mkString("( \n\t", ",\n\t", " )") )
}

object VBox
{
	def apply( boxes: List[Item] ) = new VBox( boxes, Natural )
	
	def apply( boxes: List[Item], baseline: Int ) = new VBox( boxes, Natural, baseline )
}

object VTop
{
	def apply( boxes: List[Item] ) = new VBox( boxes, Natural, 0 )
}

class ImageBox( img: BufferedImage, a: AlphaComposite ) extends GraphicsBox
{
	val kind = "Image"
	
	def isHorizontal = false

	def width = img.getWidth

	override def height = img.getHeight
	
	val ascent = height
	
	val descent = 0D
	
	def draw( g: Graphics2D, x: Double, y: Double )
	{
		g setComposite a
		g.drawImage( img, x.round.toInt, y.round.toInt, null )
	}
}

class ShapeBox( val shape: Shape, fill: Boolean, c: Color, a: AlphaComposite, l: Stroke, val kind: String, val descent: Double ) extends GraphicsBox
{
	val bounds = shape.getBounds2D
	
	def isHorizontal = false

	def width = bounds.getWidth

	override def height = bounds.getHeight
	
	val ascent = height - descent
	
	def draw( g: Graphics2D, x: Double, y: Double )
	{
	val t = g.getTransform
	
		g.translate( x - bounds.getX, y - bounds.getY )
		g setColor c
		g setComposite a
		g setStroke l
		
		if (fill)
			g fill shape
		else
			g draw shape
			
//		drawBox( g, 0, 0, RED )
		g.setTransform( t )
	}
}

object ArcBox
{
	val typeMap = Map(
		"open" -> Arc2D.OPEN,
		"chord" -> Arc2D.CHORD,
		"pie" -> Arc2D.PIE
		)
}

class ArcBox( width: Double, height: Double, start: Double, extent: Double, arctype: String, fill: Boolean, c: Color, a: AlphaComposite, l: Stroke ) extends
	ShapeBox( new Arc2D.Double(0, 0, width, height, start, extent, ArcBox.typeMap(arctype)), fill, c, a, l, "ArcBox", 0 )

class EllipseBox( width: Double, height: Double, fill: Boolean, c: Color, a: AlphaComposite, l: Stroke ) extends
	ShapeBox( new Ellipse2D.Double(0, 0, width, height), fill, c, a, l, "EllipseBox", 0 )

class CircleBox( diameter: Double, fill: Boolean, c: Color, a: AlphaComposite, l: Stroke ) extends
	EllipseBox( diameter, diameter, fill, c, a, l )

class RectangleBox( width: Double, height: Double, fill: Boolean, c: Color, a: AlphaComposite, l: Stroke ) extends
	ShapeBox( new Rectangle2D.Double(0, 0, width, height), fill, c, a, l, "RectangleBox", 0 )

class RuleBox( width: Double, ascent: Double, descent: Double, c: Color, a: AlphaComposite ) extends
	ShapeBox( new Rectangle2D.Double(0, 0, width, ascent + descent), true, c, a, Typesetter.DEFAULT_STROKE, "RuleBox", descent )

class GlyphBox( val s: String, val f: JFont, val c: Color, val a: AlphaComposite ) extends Box
{
	require( !s.isEmpty, "empty GlyphBox" )
	
	private val glyphs = f.createGlyphVector( Typesetter.FRC, s )
	val lbounds = glyphs.getLogicalBounds
	val vbounds = glyphs.getVisualBounds

	val kind = "GlyphBox( " + s + " )"
	
	val ascent = -vbounds.getY
	
	val descent = vbounds.getHeight - ascent
	
	val width = lbounds.getWidth

	val isHorizontal = true
	
	def draw( g: Graphics2D, x: Double, y: Double )
	{
		g setColor c
		g setComposite a
		g.drawGlyphVector( glyphs, x.toFloat, (y + ascent).toFloat )
	}
}

//class GlyphBox( val s: String, val f: Font, val c: Color, val a: AlphaComposite ) extends Box
//{
//	require( !s.isEmpty, "empty GlyphBox" )
//	
//	private val glyphs = f.segments( s ) map (p => p._2.createGlyphVector( Typesetter.FRC, p._1 )) map (p => (p, p.getLogicalBounds))
//	private val vbounds = glyphs map (_._1.getVisualBounds)
//
//	val kind = "GlyphBox( " + s + " )"
//	
//	val ascent = vbounds.aggregate( 0D )( _ max -_.getY, _ max _ )
//	
//	val descent = vbounds.aggregate( 0D )( (a, b) => a max (b.getHeight - -b.getY), _ max _ )
//	
//	val width = glyphs.aggregate( 0D )( _ + _._2.getWidth, _ + _ )
//
//	val isHorizontal = true
//	
//	def draw( g: Graphics2D, x: Double, y: Double )
//	{
//	var _x = x
//	val _y = (y + ascent).toFloat
//	
//		g setColor c
//		g setComposite a
//		
//		for ((gv, lb) <- glyphs)
//		{
//			g.drawGlyphVector( gv, _x.toFloat, _y )
//			_x += lb.getWidth
//		}
//	}
//}