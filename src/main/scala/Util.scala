package xyz.hyperreal.typesetter

import java.io.{File, FileInputStream}


object Util
{
	def stream( resource: Class[_], name: String ) =
		(if (resource eq null) null else resource.getResourceAsStream( name )) match
		{
		case null =>
			val file = new File( name )
			
			if (file.exists)
				new FileInputStream( name )
			else
				null
		case s => s
		}

	def replace( t: String, replacements: List[(String, String)] ) =
	{
	var ret = t
	
		for ((before, after) <- replacements)
			ret = ret.replaceAllLiterally( before, after )
	
		ret
	}
	
	def findLigatures( s: String, ligatures: List[(String, String)], exceptions: List[String] )( normal: String => Unit )( ligature: (String, String) => Unit )
	{
		def scan: Option[(Int, String, String)] =
		{
		var i = 0
		
			def skipException: Boolean =
			{
				for (e <- exceptions)
					if (s.toLowerCase.startsWith(e, i))
					{
						i += e.length
						return true
					}
				
				false
			}

			while (i < s.length - 1)
			{
				while (skipException) {}
				
				for ((g, l) <- ligatures)
					if (s.startsWith(g, i))
						return Some( i, g, l )
						
				i += 1
			}

			None
		}
	
		scan match
		{
			case None =>
				if (s != "")
					normal( s )
			case Some( (i, g, l) ) =>
				if (i > 0)
					normal( s.substring(0, i) )
					
				ligature( g, l )
				findLigatures( s.substring(i + g.length), ligatures, exceptions )( normal )( ligature )					
		}	
	}
	
	def maxWidth( l: List[List[Item]] ) = l.foldLeft( 0D )( _ max width(_) )
	
	def width( l: List[Item] ) = l.foldLeft( 0D )( (a, b) => a + (if (b.isMeasurable) b.asMeasurable.width else 0) )
	
	def maxHeight( l: List[List[Item]] ) = l.foldLeft( 0D )( _ max height(_) )
	
	def height( l: List[Item] ) = l.foldLeft( 0D )( (a, b) => a + (if (b.isMeasurable) b.asMeasurable.height else 0) )
}