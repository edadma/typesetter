package typesetter

import java.awt.geom._

import collection.mutable.{ListBuffer, ArrayBuffer}


trait Mode
{
	def add( e: Item )
	
	def string( s: String, error: String => Nothing = sys.error )
	
	def qstring( s: String )
	
	def space

	def isList = isInstanceOf[ListMode]
	
	def asList = asInstanceOf[ListMode]
	
	def isVerticalBoxMode = isInstanceOf[VerticalBoxMode]
	
	def isHorizontal = isInstanceOf[HorizontalMode]
	
	def asHorizontal = asInstanceOf[HorizontalMode]
}

abstract class ListMode( env: Typesetter ) extends Mode
{
	protected val buf = new ListBuffer[Item]
	protected var lastbox: Box = null
	protected var graphics: Boolean = false
	
	def add( e: Item )
	{
//		if (!isAllowed( e )) sys.error( "not allowed in this mode: " + e )
//		
		buf += e
		
		if (e.isGraphics)
			graphics = true
		else if (e.isBox)
		{
			lastbox = e.asBox
			graphics = false
		}
	}
	
	def space = env.spacing
	
	def empty = buf.isEmpty
	
	def last = buf.last
	
	def lastBox = lastbox
	
	def list = buf.toList
}

trait BoxMode extends Mode
{
	def box: Box
}

class HorizontalListMode( env: Typesetter ) extends ListMode( env )
{
	def string( s: String, error: String => Nothing ) = env.glyphs( s, false )
	
	def qstring( s: String ) = env.glyphs( s, true )
}

abstract class VerticalBoxMode( env: Typesetter ) extends ListMode( env ) with BoxMode
{
	override def add( e: Item )
	{
		verticalSpacing( e, env )
		super.add( e )
	}
	
	def string( s: String, error: String => Nothing )
	{
		env.indent
		env.string( s )
	}
	
	def qstring( s: String )
	{
		env.indent
		env.qstring( s )
	}
	
	override def space {}

	protected def verticalSpacing( e: Item, env: Typesetter )
	{
	val baselineskip = env.glueVariable( 'baselineskip )
	val lineskip = env.glueVariable( 'lineskip )
	val lineskiplimit = env.numberVariable( 'lineskiplimit )

		if (e.isBox && lastbox != null && !graphics)
		{
		val interbaseline = lastbox.asBox.descent + e.asBox.ascent
		val spacing = baselineskip sub interbaseline
		
			if (spacing.natural < lineskiplimit)
				add( new VSkip(lineskip) )
			else
				add( new VSkip(spacing) )
		}
	}
}

class InternalVerticalMode( size: Size, baseline: Int, env: Typesetter ) extends VerticalBoxMode( env )
{
	def isAllowed( e: Item ) = true
	
	def box = new VBox( list, size )
}

class VerticalMode( env: Typesetter ) extends VerticalBoxMode( env )
{
	def isAllowed( e: Item ) = true
	
	def box = new VBox( list, if (env.numberVariable('vsize) < 0) Natural else To(env.numberVariable('vsize)) )
}

class HorizontalMode( env: Typesetter ) extends HorizontalListMode( env )
{
	def isAllowed( e: Item ) = true
	
	private def skip[A]( s: Seq[A], from: Int, pred: A => Boolean ): Int =
	{
		require( from >= 0 )
		
		if (from >= s.length)
			s.length
		else if (pred( s(from) ))
			skip( s, from + 1, pred )
		else
			from
	}
	
	def par
	{
		if (buf isEmpty) return//sys.error( "par: empty line" )
	
	val leftskip = env.glueVariable('leftskip)
	val rightskip = env.glueVariable('rightskip)
	val parfillskip = env.glueVariable('parfillskip)
	val hangindent = env.numberVariable('hangindent)
	val hangafter = env.integerVariable('hangafter)
	val linewidth = env.numberVariable('hsize)
	val maxwidth = linewidth - leftskip.natural - rightskip.natural
	
	val lines = new ArrayBuffer[ArrayBuffer[Item]]
//	val breaks = new ArrayBuffer[Int]
	val view = buf.view.asInstanceOf[collection.SeqView[Item, ArrayBuffer[Item]]]
	var counter = 1
	
		def line( l: Seq[Item] )
		{
		var width = 0D
		
			if (!l.isEmpty)
			{
			val seq = new ArrayBuffer[Item]
			val hanging =
				if (hangafter >= 0 && counter >= hangafter + 1 || hangafter < 0 && counter <= -hangafter)
					hangindent
				else
					0
			
				def chunk( s: Seq[Item] ): Seq[Item] =
				{
				val end = skip[Item](s, skip[Item](s, 0, _.isInstanceOf[HSkip]), !_.isInstanceOf[HSkip])
				val (a, b) = s.toIndexedSeq splitAt end
				
					if (a.isEmpty)
						Seq.empty
					else
					{
						for (i <- 0 until a.length)
						{
							if (a(i).isBox)
								a(i).asBox.set
							
							if (a(i).isMeasurable)
								width += a(i).asMeasurable.width
						}
						
						if (width > maxwidth - hanging && !seq.isEmpty)
							s
						else
						{
							seq ++= a
							chunk( b )
						}
					}
				}
				
			val rem = chunk( l.dropWhile(_.isInstanceOf[HSkip]) )
			
				if (!seq.isEmpty)
					while (seq.last.isSkip)
						seq.remove( seq.length - 1 )
					
				seq.insert( 0, new HSkip(leftskip) )
				seq.insert( 1, HSpace(hanging, null) )
				seq.insert( seq.length, new HSkip(rightskip) )
				lines += seq
				counter += 1
				line( rem )
			}
		}
		
		line( view )
		lines( lines.length - 1 ).insert( lines(lines.length - 1).length - 1, new HSkip(parfillskip) )
		
		env.modePop
		
		for (l <- lines)
			env.mode.add( new HBox( l.toList, To(linewidth) ) )
	}
}

class RestrictedHorizontalMode( size: Size, env: Typesetter ) extends HorizontalListMode( env ) with BoxMode
{
	def box = new HBox( list, size )
}

object DrawMode
{
	private val referenceMap =
		Map (
			"tl" -> TopLeft,
			"ml" -> MiddleLeft,
			"bl" -> BottomLeft,
			"tc" -> TopCenter,
			"mc" -> MiddleCenter,
			"bc" -> BottomCenter,
			"tr" -> TopRight,
			"mr" -> MiddleRight,
			"br" -> BottomRight,
			"org" -> OriginReference
			)
}

class DrawMode( size: Option[(Double, Double)], env: Typesetter ) extends BoxMode
{
	val buf = new ListBuffer[Placement]
	var ref: Reference = TopLeft
	var xpos = 0D
	var ypos = 0D
	var changex = true

	def add( e: Item ) =
		e match
		{
		case b: Box =>
			buf += new Placement( b, xpos, ypos, ref )
			changex = true
			ref = TopLeft
		case _ => sys.error( "only boxes can be added in draw mode" )
		}

	def string( s: String, error: String => Nothing )
	{
		if (DrawMode.referenceMap contains s)
			ref = DrawMode.referenceMap( s )
		else
		{
		val p = env.dimen( s )
		
			if (changex)
			{
				if (p == None) error( "expected x position dimension" )
				
				xpos = p.get
				changex = false
			}
			else
			{
				if (p == None) error( "expected y position dimension" )
				
				ypos = p.get
				changex = true
			}
		}
	}
	
	override def qstring( s: String )
	{
		env.hbox
		env.glyphs( s, true )
		env.end
	}
	
	def space {}
	
	def box = new DrawBox( size, buf.toList )
}

class PathMode( env: Typesetter ) extends BoxMode
{
	val path = new Path2D.Double
	val args = new Array[Double]( 6 )
	var initial = true
	var oper: String = null
	var argc: Int = _
	var index: Int = _
	
	val argMap = Map( "move" -> 2, "line" -> 2, "quad" -> 4, "curve" -> 6 )
	
	def add( e: Item ) = sys.error( "nothing can be added in path mode" )
	
	def string( s: String, error: String => Nothing )
	{
		if (s matches "move|line|quad|curve|close")
		{
			if (oper ne null) error( "no operation expected" )
			
			if (initial && s != "move") error( "expected 'move' operation" )
			
			if (s == "close")
			{
				path.closePath
				initial = true
			}
			else
			{
				initial = false
				oper = s
				argc = argMap(s)
				index = 0
			}
		}
		else
		{
			env.dimen( s ) match
			{
				case None =>
					if (oper ne null)
						error( "expected coordinate dimension" )
					else
						error( "illegal string" )
						
				case Some( d ) =>
					if (oper eq null) error( "no dimension expected" )
					
					args(index) = d
					index += 1
					
					if (index == argc)
					{
						oper match
						{
							case "move" => path.moveTo( args(0), args(1) )
							case "line" => path.lineTo( args(0), args(1) )
							case "quad" => path.quadTo( args(0), args(1), args(2), args(3) )
							case "curve" => path.curveTo( args(0), args(1), args(2), args(3), args(4), args(5) )
						}
						
						oper = null
					}
			}
		}
	}
	
	def qstring( s: String ) = sys.error( "quoted strings in path mode is a reserved feature" )
	
	def end( error: String => Nothing )
	{
//		val it = path.getPathIterator( null )
//		val coords = new Array[Double]( 6 )
//		
//		while (!it.isDone)
//		{
//			println( it.currentSegment(coords) )
//			println( coords.mkString(", ") )
//			it.next
//		}
//		
		if (oper ne null) error( "missing " + (argc - index) + " path operation argument(s)" )
	}
	
	def space {}
	
	def box = new ShapeBox( path, false, env.color, env.alpha, env.stroke, "Path", 0 )
}