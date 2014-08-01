package typesetter

import java.awt.{Font => JFont, Color, GraphicsEnvironment, AlphaComposite, Stroke, BasicStroke}
import java.awt.Font._
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform
import java.io.{InputStream, FileInputStream}
import java.util.regex._

import collection.mutable.{ListBuffer, ArrayStack}
import collection.immutable.{HashMap => ImmutableHashMap}

import Util._


class Typesetter
{
	type Dimen = Double
	
	protected val modeStack = new ArrayStack[Mode]
	protected val variablesStack = new ArrayStack[ImmutableHashMap[Symbol, Any]]
	
	push( Typesetter.DEFAULT_VARIABLES )

	def push( variables: ImmutableHashMap[Symbol, Any] ) = variablesStack.push( variables )
	
	def push = variablesStack.push( new ImmutableHashMap )
	
	def pop = variablesStack.pop
	
	def variable = variablesStack.top.asInstanceOf[Map[Symbol, Any]]
	
	def level = variablesStack.size
	
	def variable( key: Symbol, value: Any )
	{
		if (key == 'font)
		{
		val t = value.asInstanceOf[Font]
		
			variable( 'baselineskip, t.baseline )
			variable( 'spaceskip, t.space )
			variable( 'cspaceskip, t.cspace )
			variable( 'xspaceskip, t.xspace )
		}
		
		push( pop + (key -> value) )
	}
	
	def remove( key: Symbol ) = push( pop - key )
	
	def assigned( key: Symbol ) = variablesStack.top contains key
	
	def vars = println( variable.mkString("variables:\n\t", ",\n\t", "\n") )
	
	def apply( key: Symbol ) = variable( key )
	
	def update( key: Symbol, value: Any ) = variable( key, value )
	
	def integerVariable( key: Symbol ) = variable(key).asInstanceOf[Int]
	
	def numberVariable( key: Symbol ) = variable(key).asInstanceOf[Number].doubleValue
	
	def glueVariable( key: Symbol ) = variable(key).asInstanceOf[Glue]
	
	def booleanVariable( key: Symbol ) = variable(key).asInstanceOf[Boolean]
	
	def colorVariable( key: Symbol ) = variable(key).asInstanceOf[Color]
	
	def fontVariable( key: Symbol ) = variable(key).asInstanceOf[Font]
	
	def boxVariable( key: Symbol ) =
		variable get key match
		{
		case None => None
		case Some( b: Box ) => Some( b )
		case _ => sys.error( key.name + " is not a box variable" )
		}
	
	def dup = push( variablesStack.top )
	
	def modePop = modeStack.pop

	def in = numberVariable( 'dpi )
	
	def pt = in/72
	
	def cm = in/2.54
	
	def dimen( s: String ): Option[Dimen] =
		if (Typesetter.DIMEN_REGEX.pattern.matcher( s ).matches)
		{
		val Typesetter.DIMEN_REGEX( m, bd, bn, d ) = s

			if ((m == null || m == "-") && bd == null && d == null)
				None
			else
			{
			val mul = if (m eq null) 1 else if (m == "-") -1 else m.toDouble
			val dim =
				if (bd eq null)
					d match
					{
						case "em" => font.em
						case "ex" => font.ex
						case "px"|null => 1
						case "pt" => pt
						case "in" => in
						case "cm" => cm
						case _ =>
							if (variable.contains( Symbol(d) ))
								numberVariable( Symbol(d) )
							else
								return None
					}
				else
				{
				val b =
					boxVariable( Symbol(bn) ) match
					{
					case None => return None
					case Some( b ) => b
					}
				
					bd match
					{
						case "wd" => b.width
						case "as" => b.ascent
						case "ds" => b.descent
						case "ht" => b.height
					}
				}
			
				Some( mul*dim )
			}
		}
		else
			None

	def glue( s: String ): Option[Glue] =
		if (Typesetter.GLUE_REGEX.pattern.matcher( s ).matches)
		{
		val Typesetter.GLUE_REGEX( m, d ) = s
		
			if (m == null && d == null)
				None
			else
			{
			val mul = if (m eq null) 1 else m.toDouble
			
				d match
				{
					case "em" => Some( Glue(mul*font.em) )
					case "ex" => Some( Glue(mul*font.ex) )
					case "px"|null => Some( Glue(mul) )
					case "pt" => Some( Glue(mul*pt) )
					case "in" => Some( Glue(mul*in) )
					case "cm" => Some( Glue(mul*cm) )
					case _ =>
						variable get Symbol(d) match
						{
							case Some( v: Dimen ) => Some( Glue(mul*v) )
							case Some( v: Glue ) => Some( v.mul(mul) )
							case _ => None
						}
				}			
			}
		}
		else
			None

	def vbox {vbox( Natural )}
	
	def vbox( size: Size ) {vbox( size, -1 )}
	
	def vbox( size: Size, baseline: Int )
	{
		dup
		modeStack.push( new InternalVerticalMode(size, baseline, this) )
	}
	
	def vtop {vtop( Natural )}
	
	def vtop( size: Size ) {vbox( Natural, 0 )}
	
	def horizontal
	{
		if (!mode.isHorizontal)
		{
			if (!mode.asList.empty)
				vskip( 'parskip )
				
			modeStack.push( new HorizontalMode(this) )
		}
	}
	
	def vertical
	{
		dup
		modeStack.push( new VerticalMode(this) )
	}
	
	def hbox {hbox( Natural )}
	
	def hbox( size: Size )
	{
		dup
		modeStack.push( new RestrictedHorizontalMode(size, this) )
	}

	def empty( width: Double, height: Double ) = add( Space(width, height) )
	
	def add( e: Item )
	{
		mode.add( e )
	}

	def add( l: List[Item] ) {l foreach add}
	
	def addHorizontal( e: Item )
	{
		paragraph
		add( e )
	}
	
	def addVertical( e: Item )
	{
		par
		add( e )
	}
	
	def +=( e: Item ) = add( e )
	
	def font( t: Font ) = variable( 'font, t )
	
	def font = variable('font).asInstanceOf[Font]
	
	def color( c: Color ) = variable( 'color, c )
	
	def color = variable('color).asInstanceOf[Color]
	
	def alpha( a: AlphaComposite ) = variable( 'alpha, a )
	
	def alpha = variable('alpha).asInstanceOf[AlphaComposite]
	
	def stroke( l: Stroke ) = variable( 'stroke, l )
	
	def stroke = variable('stroke).asInstanceOf[Stroke]
	
	def hrule( width: Double, ascent: Double, descent: Double ) = addVertical( new HRule(Some(width), ascent, descent, color, alpha) )
	
	def hrule( thickness: Double ) = addVertical( HRule(this, thickness) )
	
	def hrule( ascent: Double, descent: Double ) = addVertical( HRule(this, ascent, descent) )
	
	def vrule( thickness: Double ) = addHorizontal( VRule(this, thickness) )
	
	def hskip( key: Symbol ) {hskip( glueVariable(key) )}
	
	def hskip( g: Glue ) = addHorizontal( new HSkip(g) )
	
	def hskip( width: Double ) {hskip( Glue(width) )}
	
	def vskip( key: Symbol ) {vskip( glueVariable(key) )}
	
	def vskip( g: Glue ) = addVertical( new VSkip(g) )
	
	def vskip( height: Double ) {vskip( Glue(height) )}
	
	def hfil = addHorizontal( new HFil )
	
	def vfil = addVertical( new VFil )
	
	def hfilneg = addHorizontal( new HFilneg )
	
	def vfilneg = addVertical( new VFilneg )
	
	def hss = addHorizontal( new Hss )
	
	def vss = addVertical( new Vss )
	
	def hspace( px: Double ) = add( HSpace(px, null) )
	
//	def vspace( px: Double ) = add( VSpace(px) )
	
	def space = mode.space
	
	def string( s: String, error: String => Nothing = sys.error ) = mode.string( s, error )
	
	def qstring( s: String ) = mode.qstring( s )
	
	def spacing =
		if (!empty && last.isInstanceOf[GlyphBox])
		{
		val s = last.asInstanceOf[GlyphBox].s
	
			if (s.last == ',' || s.last == ';')
				hskip( 'cspaceskip )
			else if (Typesetter.XSPACE_PATTERN.matcher( s ).matches && !Typesetter.XSPACE_EXCEPTION_PATTERN.matcher( s ).matches)
				hskip( 'xspaceskip )
			else
				interword
		}
		else
			interword
	
	def interword = hskip( 'spaceskip )

	def text( t: String )
	{
	val words = t.trim.split( "\\s+" )
	
		for (i <- 0 until words.length)
		{
			if (i > 0)
				space
				
			string( words(i) )
		}
	}
	
	def glyph( c: Char )
	{
	val (s, f) = font.segments( c.toString ).head
	
		glyphbox( s, f )
	}
	
	def glyphs( s: String, literal: Boolean )
	{
	val ligatures = booleanVariable( 'enableligatures )
	val monospaced = font.monospaced
	
			for ((seg, f) <- font.segments( s ))
				if (ligatures && !monospaced && !literal)
					findLigatures( seg, Typesetter.LIGATURES, Typesetter.EXCEPTIONS )
						{sec => glyphbox( replace(sec, Typesetter.REPLACEMENTS), f )} {(sec, lig) => glyphbox( lig, f )}
				else
					glyphbox( seg, f )
	}
	
	def glyphbox( s: String, f: JFont ) = addHorizontal( new GlyphBox(s, f, color, alpha) )

//	def ligaturebox( s: String, ligature: String ) = add( LigatureBox(this, s, ligature) )
	
	def shift( b: Box, px: Double ) = add( new Shift(b, px) )

	def shift( s: String, px: Double )
	{
		paragraph
		hbox
		glyphs( s, false )
		shift( box, px )
	}
	
	def kern( px: Double ) =
		if (mode.isVerticalBoxMode)
			add( VKern(px) )
		else
			add( HKern(px) )

	def box =
	{
		par
		
	val res = mode.asInstanceOf[BoxMode].box
	
		modePop
		pop
		res
	}
	
	def mode = modeStack.top
	
	def empty = mode.asList.empty
	
	def last = mode.asList.last
	
	def list
	{
		dup
		modeStack.push( new HorizontalListMode(this) )
	}
	
	def arg =
	{
	val res = mode.asList.list
	
		modePop
		pop
		res
	}
	
	def arg( t: String ): List[Item] =
	{
		list
		text( t )
		arg
	}
	
	def end = add( box )

	def move( b: Box, px: Double ) = add( new Move(b, px) )

	def par =
		if (mode.isHorizontal)
		{
			mode.asHorizontal.par
			variable( 'hangindent, 0 )
			variable( 'hangafter, 1 )
		}
	
	def endParagraph =
	{
		par
		
		if (mode.isHorizontal)
			modePop
	}
	
	def paragraph =
		if (mode.isVerticalBoxMode)
			indent
	
	def indent
	{
		if (mode.isVerticalBoxMode)
			horizontal
			
//		if (booleanVariable('autoindent))
			hspace( numberVariable('parindent) )
	}
	
	def accent( s: String, combaccent: String, accentmap: Map[Char, Char] ) =
	{
	val first = s.charAt( 0 )
	val rest = s.substring( 1 )
	
		accentmap.get( s.charAt(0) ) match
		{
		case Some( newchar ) => newchar + rest
		case None => first + combaccent + rest
		}
	}
	
	def showlast
	{
		println( last )
	}
	
	//
	// Graphics
	//
	
	def draw( size: Option[(Double, Double)] )
	{
		dup
		modeStack.push( new DrawMode(size, this) )
	}
	
	def path
	{
		dup
		modeStack.push( new PathMode(this) )
	}
	
	def circle( diameter: Double, fill: Boolean ) = add( new CircleBox(diameter, fill, color, alpha, stroke) )
	
	def rectangle( width: Double, height: Double, fill: Boolean ) = add( new RectangleBox(width, height, fill, color, alpha, stroke) )
	
	def ellipse( width: Double, height: Double, fill: Boolean ) = add( new EllipseBox(width, height, fill, color, alpha, stroke) )
	
	def arc( width: Double, height: Double, start: Double, extent: Double, arctype: String, fill: Boolean ) = add( new ArcBox(width, height, start, extent, arctype, fill, color, alpha, stroke) )
	
	//
	// simple format support
	//
	
	def hang
	{
		variable( 'hangindent, numberVariable('parindent) )
	}
	
	def textindent( l: List[Item] )
	{
		indent
		list
		add( l )
		hskip( 10 )
		llap( arg )
	}
	
	def item( l: List[Item] )
	{
		par
		hang
		textindent( l )
	}
	
	def itemitem( l: List[Item] )
	{
		par
		indent
		variable( 'hangindent, 2*numberVariable('parindent) )
		textindent( l )
	}
	
	def line = hbox( To(numberVariable('hsize)) )
	
	def centerline( l: List[Item] )
	{
		line
		hfil
		add( l )
		hfil
		end
	}
	
	def leftline( l: List[Item] )
	{
		line
		add( l )
		hfil
		end
	}
	
	def rightline( l: List[Item] )
	{
		line
		hfil
		add( l )
		end
	}
	
	def llap( l: List[Item] )
	{
		hbox( To(0) )
		hss
		add( l )
		end
	}

	def rlap( l: List[Item] )
	{
		hbox( To(0) )
		add( l )
		hss
		end
	}

	def underline( l: List[Item], downward: Double, thickness: Double )
	{
		hbox
		add( l )
		kern( -Util.width(l) )
		hrule( Util.width(l), -downward, thickness + downward )
		end
	}

	def underline( s: String ) {underline( arg(s), 3, 2 )}
	
	def frame( l: List[Item], space: Double, thickness: Double ) =
		add( HBox( List(
			VRule(this, thickness),
			VTop( List(
				VBox( List(
					HRule(this, thickness),
					VSkip(space),
					HBox(
						HSkip(space) +: l :+ HSkip(space)
						)
					)),
				VSkip(space),
				HRule(this, thickness))),
				VRule(this, thickness)
				)) )

	def frame( s: String, space: Double, thickness: Double ) {frame( arg(s), space, thickness )}

	def frame( s: String ) {frame( s, 3, 1 )}
	
	def translucent( b: Box, margin: Double )
	{
		add( new TranslucentBox(b, margin, .5) )
	}
	
	def halign( size: Size, table: List[List[List[Item]]], margin: Glue, intercolumn: List[Glue] )
	{
		if (!table.isEmpty)
		{
		val cols = table.aggregate( 0 )( _ max _.length, _ max _ )
		val buf = new ListBuffer[List[List[Item]]]
		
			for (r <- table)
				buf +=
					(if (r.length < cols)
						r ++ List.fill( cols - r.length )( Nil )
					else
						r)
		
		val table1 = buf.toList
		val intercolumn1 =
			if (intercolumn.length > cols + 1)
				intercolumn take cols + 1
			else if (intercolumn.length < cols + 1)
				intercolumn ++ List.fill( cols + 1 - intercolumn.length )( ZGlue )
			else
				intercolumn
		val widths = table1.transpose map (Util.maxWidth( _ ))
		
			for (row <- table1)
			{
			val items = ListBuffer[Item]( new HSkip(margin) )
			
				for (((e, w), g) <- row zip widths zip intercolumn1)
				{
					items += new HBox( e, To(w) )
					items += new HSkip( g )
				}
			
				addVertical( new HBox(items.toList) )
			}
		}
	}
}

object Typesetter
{
	val DIMEN_REGEX = """(-?(?:\d+(?:\.\d*)?|\d*\.\d+)(?:(?:e|E)-?\d+)?|-)?(?:(wd|as|ds|ht)(\d+)|([a-zA-Z]+))?"""r
	val GLUE_REGEX = """(-?(?:\d+(?:\.\d*)?|\d*\.\d+)(?:(?:e|E)-?\d+)?)?([a-zA-Z]+)?"""r
	
	//
	// Unicode character definitions
	val `EM SPACE` = "\u2003"
	val `left single quotation mark` = "\u2018"
	val `right single quotation mark` = "\u2019"
	val `left double quotation mark` = "\u201C"
	val `right double quotation mark` = "\u201D"
	val `left-pointing angle bracket` = "\u2329"
	val `right-pointing angle bracket` = "\u232A"

	val hyphen = "\u2010"
	val `EN DASH` = "\u2013"
	val `EM DASH` = "\u2014"
	
	val `LEFTWARDS ARROW` = "\u2190"
	val `RIGHTWARDS ARROW` = "\u2192"
	val `LEFT RIGHT ARROW` = "\u2194"
	val `LEFTWARDS DOUBLE ARROW` = "\u21D0"
	val `RIGHTWARDS DOUBLE ARROW` = "\u21D2"
	val `LEFT RIGHT DOUBLE ARROW` = "\u21D4"

	val `LONG LEFTWARDS ARROW` = "\u27F5"
	val `LONG RIGHTWARDS ARROW` = "\u27F6"
	val `LONG LEFT RIGHT ARROW` = "\u27F7"
	val `LONG LEFTWARDS DOUBLE ARROW` = "\u27F8"
	val `LONG RIGHTWARDS DOUBLE ARROW` = "\u27F9"
	val `LONG LEFT RIGHT DOUBLE ARROW` = "\u27Fa"
	
	val BULLET = "\u2022"
	val `TRIANGULAR BULLET` = "\u2023"

	val `LATIN SMALL LIGATURE FF` = "\uFB00"
	val `LATIN SMALL LIGATURE FI` = "\uFB01"
	val `LATIN SMALL LIGATURE FL` = "\uFB02"
	val `LATIN SMALL LIGATURE FFI` = "\uFB03"
	val `LATIN SMALL LIGATURE FFL` = "\uFB04"
	
	val `LATIN CAPITAL LETTER L WITH STROKE` = "\u0141"
	val `LATIN SMALL LETTER L WITH STROKE` = "\u0142"
		
	val `LATIN CAPITAL LIGATURE OE` = "\u0152"
	val `LATIN SMALL LIGATURE OE` = "\u0153"
		
	val `INVERTED EXCLAMATION MARK` = "\u00A1"
	val `INVERTED QUESTION MARK` = "\u00BF"
	
	val `LESS-THAN SIGN` = "\u003C"
	val `GREATER-THAN SIGN` = "\u003E"
	val `DEGREE SIGN` = "\u00B0"
	val `PLUS-MINUS SIGN` = "\u00B1"
	val `MIDDLE DOT` = "\u00B7"
	val `LATIN CAPITAL LETTER AE` = "\u00C6"
	val `LATIN SMALL LETTER AE` = "\u00E6"
	val `MULTIPLICATION SIGN` = "\u00D7"
	val `LATIN CAPITAL LETTER O WITH STROKE` = "\u00D8"
	val `DIVISION SIGN` = "\u00F7"
	val `LATIN SMALL LETTER O WITH STROKE` = "\u00F8"
		
//	val `LATIN SMALL LETTER A WITH ACUTE` = '\u00E1'
//	val `LATIN CAPITAL LETTER A WITH ACUTE` = '\u00C1'
//	val `LATIN SMALL LETTER E WITH ACUTE` = '\u00E9'
//	val `LATIN CAPITAL LETTER E WITH ACUTE` = '\u00C9'
	
	val `LATIN SMALL LETTER Y WITH DIAERESIS` = "\u00FF"
		
	val `POUND SIGN` = "\u00A3"
	val `COPYRIGHT SIGN` = "\u00A9"
	val `HORIZONTAL ELLIPSIS` = "\u2026"
	
	val `COMBINING GRAVE ACCENT` = "\u0300"
	val `COMBINING ACUTE ACCENT` = "\u0301"
	val `COMBINING CIRCUMFLEX ACCENT` = "\u0302"
	val `COMBINING TILDE` = "\u0303"
	val `COMBINING MACRON` = "\u0304"
	val `COMBINING DIAERESIS` = "\u0308"
	val `COMBINING RING ABOVE` = "\u030A"
	val `COMBINING CEDILLA` = "\u0327"
	
	val XSPACE_PATTERN = ".*[^A-Z][.!?:].{0,2}".r.pattern
	val XSPACE_EXCEPTION_PATTERN = """(Mr|Mrs|Ms|Dr|Jr|Sr|Atty|Prof|Hon|Pres|Rep|Sir|Adv|Sen|Gov|Amb|Sec|Pvt|Pfc|Cpl|Spec|Sgt|Ens|Adm|Maj|Capt|Cmdr|Cdr|Lt|Col|Gen)\.""".r.pattern
	val FRC = new FontRenderContext( null, true, false )
	val DEFAULT_ALPHACOMPOSITE = AlphaComposite.Src
	val DEFAULT_STROKE = new BasicStroke
	val DEFAULT_FONT = Font( null, "Serif", "plain", 20 )
	val DEFAULT_TRANSFORM = new AffineTransform
	val DEFAULT_VARIABLES = ImmutableHashMap(
			'baselineskip -> DEFAULT_FONT.baseline,
			'lineskip -> Glue( 1 ),
			'lineskiplimit -> 0,
			'spaceskip -> DEFAULT_FONT.space,
			'cspaceskip -> DEFAULT_FONT.cspace,
			'xspaceskip -> DEFAULT_FONT.xspace,
			'hsize -> 1280,
			'vsize -> 720,
			'parindent -> 55,
			'parfillskip -> FGlue,
			'leftskip -> ZGlue,
			'rightskip -> ZGlue,
			'parskip -> Glue( 0, 1, 0, 0 ),
			'hangindent -> 0,
			'hangafter -> 1,
			'tabskip -> Glue( 20 ),
			
			'transform -> DEFAULT_TRANSFORM,
			'dpi -> 91.5,
			'font -> DEFAULT_FONT,
			'color -> Color.BLACK,
			'alpha -> DEFAULT_ALPHACOMPOSITE,
			'stroke -> DEFAULT_STROKE,
			'enableligatures -> true,
			'autoindent -> true
		)
	
	val LIGATURES = List(
			"ffi" -> `LATIN SMALL LIGATURE FFI`,
			"ffl" -> `LATIN SMALL LIGATURE FFL`,
			"ff" -> `LATIN SMALL LIGATURE FF`,		//disabled because the Java "Serif" font messes this one up
			"fi" -> `LATIN SMALL LIGATURE FI`,
			"fl" -> `LATIN SMALL LIGATURE FL`,
//			"OE" -> `LATIN CAPITAL LIGATURE OE`,
//			"Oe" -> `LATIN CAPITAL LIGATURE OE`,
//			"oe" -> `LATIN SMALL LIGATURE OE`,
//			"AE" -> `LATIN CAPITAL LETTER AE`,
//			"Ae" -> `LATIN CAPITAL LETTER AE`,
//			"ae" -> `LATIN SMALL LETTER AE`,
			"``" -> `left double quotation mark`,
			"''" -> `right double quotation mark`,
			"---" -> `EM DASH`,
			"<-->" -> `LONG LEFT RIGHT ARROW`,
			"<==>" -> `LONG LEFT RIGHT DOUBLE ARROW`,
			"<--" -> `LONG LEFTWARDS ARROW`,
			"-->" -> `LONG RIGHTWARDS ARROW`,
			"<==" -> `LONG LEFTWARDS DOUBLE ARROW`,
			"==>" -> `LONG RIGHTWARDS DOUBLE ARROW`,
			"<->" -> `LEFT RIGHT ARROW`,
			"<=>" -> `LEFT RIGHT DOUBLE ARROW`,
			"<-" -> `LEFTWARDS ARROW`,
			"->" -> `RIGHTWARDS ARROW`,
			"<=" -> `LEFTWARDS DOUBLE ARROW`,
			"=>" -> `RIGHTWARDS DOUBLE ARROW`,
			"--" -> `EN DASH`,
			"..." -> `HORIZONTAL ELLIPSIS`
		)

	val EXCEPTIONS = List(
			"fful",
			"fing", "fish", "fier", "fily", "finess",
			"fless", "fly", "flike", "flife", "fline", "flet", "pdflatex",
			"ffing", "ffish", "ffier", "ffily", "ffiness", "ffies", "ffian",
			"ffly", "ffless", "scofflaw", "cufflink", "offline", "offload", "fflike",
			"chaffinch", "wolffish",
			"safflower",
			"fteen", "fth", "ftie", "fty", "halftime", "halftone", "rooftop", "rooftree",
			"offtrack" )
//			"doer", "does", "shoe", "toe", "soever", "joe" )

	val REPLACEMENTS = List(
			"`" -> `left single quotation mark`,
			"'" -> `right single quotation mark`,
			"<" -> `left-pointing angle bracket`,
			">" -> `right-pointing angle bracket`,
			"-" -> hyphen
		)
		
	val accentPattern = List( `COMBINING GRAVE ACCENT`, `COMBINING ACUTE ACCENT`, `COMBINING CIRCUMFLEX ACCENT`, `COMBINING DIAERESIS` )
		
	val commonAccentMap = Map(
			'A' -> (List(`COMBINING GRAVE ACCENT`, `COMBINING ACUTE ACCENT`, `COMBINING CIRCUMFLEX ACCENT`, `COMBINING TILDE`, `COMBINING DIAERESIS`, `COMBINING RING ABOVE`), 0xC0),
			'O' -> (List(`COMBINING GRAVE ACCENT`, `COMBINING ACUTE ACCENT`, `COMBINING CIRCUMFLEX ACCENT`, `COMBINING TILDE`, `COMBINING DIAERESIS`), 0xD2),
			'U' -> (accentPattern, 0xD9),
			'I' -> (accentPattern, 0xCC),
			'E' -> (accentPattern, 0xC8),
			'N' -> (List(`COMBINING TILDE`), 0XD1),
			'Y' -> (List(`COMBINING ACUTE ACCENT`), 0xDD),
			'C' -> (List(`COMBINING CEDILLA`), 0xC7)
		)
	
	def commonAccent( s: String, a: String ) =
	{
	val first = s.charAt( 0 )
	val rest = s.substring( 1 )
	lazy val combining = first + a + rest
	
		if (first == 'y' && a == `COMBINING DIAERESIS`)
			`LATIN SMALL LETTER Y WITH DIAERESIS` + rest
		else
			commonAccentMap.get(first.toUpper) match
			{
			case None => combining
			case Some( (p, s) ) =>
				p.zipWithIndex find (e => e._1 == a) match
				{
				case None => combining
				case Some( (_, index) ) => (s + (if (first.isLower) 0x20 else 0) + index).toChar + rest
				}
			}
	}
	
	def macronAccent( s: String ) = s.charAt( 0 ) + `COMBINING MACRON` + s.substring( 1 )
}

class TypesetterException( m: String ) extends Exception( m )
