package xyz.hyperreal.typesetter

import java.awt.{Font => JFont, GraphicsEnvironment}
import java.awt.Font._
import java.awt.geom.AffineTransform
import java.util.regex.Pattern

import collection.mutable.{ListBuffer, HashMap}

//trait Style
//case object PlainStyle extends Style
//case object ItalicStyle extends Style
//case object SlantStyle extends Style
//case object BoldStyle extends Style

object Font
{
	private val jfontMap = new HashMap[String, JFont]
	private val families = Set( GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames: _* )
	
	private val styleMap =
		Map(
			"plain" -> PLAIN,
			"bold" -> BOLD,
			"slant" -> ITALIC
			)
	val FONT_NAME = Pattern.compile( """.*?([a-zA-Z0-9_-]+)\.[a-zA-Z]+""" )

//	var resource: Class[_] = null
	
	def jfonts = jfontMap.keySet
	
	def fromName( resource: Class[_], name: String, style: String, size: Int ) =
	{
	val f = jfont( resource, name, if (style == "smallcaps") "plain" else style, size )

		if (f == None)
			None
		else
			Some( if (style == "smallcaps")
				smallcaps( name, size, f.get )
			else
				new BasicFont( name, style, size, f.get ) )
	}
	
	def apply( resource: Class[_], name: String, style: String, size: Int ) =
		fromName( resource, name, style, size ) match
		{
		case None => sys.error( "font not found: " + name )
		case Some( f ) => f
		}
	
	def preload( resource: Class[_], name: String ) =
		if (load( resource, name ) == None)
			sys.error( "font not found: " + name )
			
//	def smallcaps( name: String, style: Int, size: Int ) =
//	{
//	val jfont = Typesetter.jfont( name, style, size )
//	val small = jfont.deriveFont( AffineTransform.getScaleInstance(.85, .75) )
//	
//		new CharRangeFont( name, style, size, jfont, ('a', 'z', 'A', small) )
//	}
	
	def smallcaps( name: String, size: Int, f: JFont ) =
	{
	val small = f.deriveFont( BOLD ).deriveFont( AffineTransform.getScaleInstance(.85, .76) )
	
		new CharRangeFont( name, "smallcaps", size, f, ('a', 'z', 'A', small) )
	}
	
//	def compact( name: String, style: String, size: Int ) = new BasicFont( name, style, size, jfont(name, style, size).
//		deriveFont(AffineTransform.getScaleInstance(.8, 1)) )
	
	def jfont( resource: Class[_], name: String, style: String, size: Int ): Option[JFont] = jfont( resource, name, styleMap(style), size )
	
	def jfont( resource: Class[_], name: String, style: Int, size: Int ) =
		load( resource, name ) match
		{
		case Some( f ) => Some( f.deriveFont(style, size.toFloat) )
		case None => None
		}

	def load( resource: Class[_], name: String ) =
		jfontMap.get(name) match
		{
			case None =>
				if (families contains name)
					Some( JFont.decode(name) )
				else
				{
				val s = Util.stream( resource, name )			
				
					if (s eq null)
						None
					else {
					val jfont = JFont.createFont( if (name.toLowerCase.endsWith( ".ttf" ) || name.toLowerCase.endsWith( ".otf" )) TRUETYPE_FONT else TYPE1_FONT, s )
					
						s.close
						jfontMap(name) = jfont
						
					val m = FONT_NAME.matcher( name )
	
						if (m.matches)
							jfontMap(m.group(1)) = jfont
						
						Some( jfont )
					}
				}
			case f => f
		}
}

abstract class Font( val name: String, val style: String, val size: Int )
{
	def charbox( c: String ) = new GlyphBox( c, segments(c).head._2, null, null )
	
	val em = charbox( Typesetter.`EM SPACE` ).width

	val ex = charbox( "x" ).ascent

	val (ascent, descent, height) =
		{
		val b = charbox( "[" )
		
			(b.ascent, b.descent, b.height)
		}
	
	val capital = charbox( "T" ).ascent
	
	val monospaced = (em - charbox( "." ).width).abs < .01
	
	val space = Glue( if (monospaced) em else em*0.333, em*0.15, 0, em*0.1 )

	val cspace = Glue( if (monospaced) em else em*0.333, em*0.25, 0, em*0.1 )
	
	val xspace = Glue( if (monospaced) 2*em else em*0.45, em*0.3, 0, em*0.05 )

	val baseline = Glue( (height*1.3).ceil )
	
	def segments( s: String ): List[(String, JFont)]
	
//	private val styles = Map( PLAIN -> "plain", BOLD -> "bold", ITALIC -> "italic" )
	
	override def toString = "<Font: " + name + " " + style + ">"
}

class BasicFont( name: String, style: String, size: Int, jfont: JFont ) extends Font( name, style, size )
{
	def segments( s: String ) = List( (s, jfont) )
}

class CharRangeFont( name: String, style: String, size: Int, jfont: JFont, ranges: (Char, Char, Char, JFont)* ) extends Font( name, style, size )
{
	def segments( s: String ) =
	{
	val buf = new ListBuffer[(String, JFont)]
		
		def lookup( c: Char ) =
			ranges.find( r => c >= r._1 && c <= r._2 ) match
			{
				case None => (c, jfont)
				case Some( (l, _, b, f) ) => ((b + (c - l)).toChar, f)
			}

	val seg = new StringBuilder
	var (_, font) = lookup( s(0) )
	
		def segment = buf += ((seg.toString, font))
	
		for (i <- 0 until s.length)
		{
		val (c, f) = lookup( s(i) )
		
			if (f ne font)
			{
				segment
				seg.clear
				font = f
			}

			seg += c			
		}
	
		segment
		
		buf.toList
	}
}
