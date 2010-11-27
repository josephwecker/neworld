package gamelib2d;

// BMFont - load and use fonts created with AngelCode Bitmap Font Generator (export as binary + png)

import flash.geom.ColorTransform;
import flash.display.BitmapData;
import flash.display.BlendMode;
import flash.geom.Matrix;
import flash.geom.Point;
import flash.geom.Rectangle;
import flash.utils.ByteArray;
import flash.utils.Endian;
import haxe.io.Bytes;
import haxe.Resource;

//
// Project, Properties, Compiler Options, Additional Compiler Options:
//
//   -resource XXX.fnt@XXX
//
//
// Res.xml (between <frame> ... </frame>:
//
//      <library>
//        <bitmap id="XXX_00" name="XXX_00" import="XXX_00.png"/>
//      </library>
//
//
// Main.hx:
//
//   class XXX_00 extends BitmapData
//   {
//       public function new ()
// 	     {
//           super (0, 0);
//       }
//   } 
//

class BMFont
{
	public static inline var ALIGN_LEFT: Int = -1;
	public static inline var ALIGN_CENTER: Int = 0;
	public static inline var ALIGN_RIGHT: Int = 1;
	
	public var baFnt: ByteArray;
	public var bmfVersion: Int;
	public var abdImages: Array<BitmapData>;
	
	public var kerning: Bool;
	public var spacingH: Int;
	public var spacingV: Int;
	public var shadow: Bool;
	public var shadowX: Int;
	public var shadowY: Int;
	public var align: Int;
	
	// basic info
	public var fontSize: Int;
	public var bold: Bool;
	public var italic: Bool;
	public var unicode: Bool;
	public var smooth: Bool;
	public var charSet: Int;
	public var stretchH: Int;
	public var aa: Int;
	public var paddingUp: Int;
	public var paddingRight: Int;
	public var spacingHoriz: Int;
	public var spacingVert: Int;
	public var outline: Int;
	
	// common
	public var lineHeight: Int;
	public var base: Int;
	public var scaleX: Int;
	public var scaleY: Int;
	public var pages: Int;
	public var packed: Bool;
	public var encoded: Bool;
	public var alphaChannel: Int;
	public var redChannel: Int;
	public var greenChannel: Int;
	public var blueChannel: Int;

	// characters
	public var idStr: String;
	public var id: Array<Int>;
	public var x: Array<Int>;
	public var y: Array<Int>;
	public var width: Array<Int>;
	public var height: Array<Int>;
	public var offsetX: Array<Int>;
	public var offsetY: Array<Int>;
	public var advanceX: Array<Int>;
	public var page: Array<Int>;
	
	public var ascii: Array<Int>;
	
	public var kerningChars: Array<String>;
	public var kerningValue: Array<String>;
	
	
	public var skipChars: String;
	
	public var blendMode: BlendMode;
	public var colorTransform: ColorTransform;
	
	
	
	public function textHeight (s: String): Int
	{ 
		var h: Int = 1;
		for (i in 0...s.length)
			if (s.charCodeAt (i) == 13)
				h++;
		return h * (fontSize + spacingVert + spacingV);
	}
	
	public function textWidth (s: String, ?line = -1): Int
	{
		var m: Int = 0;
		var l: Int = 0;
		var index: Int;
		var lastIndex: Int = -1;
		var fdx: Int = 0;
		for (i in 0...s.length)
		{
			var asc: Int = s.charCodeAt (i);
			if (asc == 13)
			{
				m = Utils.iMax (m, fdx);
				fdx = 0;
				lastIndex = -1;
				l++;
			}
			else
			{
				if (line == -1 || l == line)
				{
					index = findChar (asc);
					if ((index >= 0) && (skipChars.indexOf (String.fromCharCode (asc)) == -1))
					{
						if (false && kerning)
						{
							if (lastIndex != -1)
							{
								var k: Int = kerningChars[lastIndex].indexOf (String.fromCharCode (asc));
								if (k >= 0)
									fdx += signedByte (kerningValue[lastIndex].charCodeAt (k) & 0xFF);
							}
						}
						fdx += signedShort (advanceX[index]);
						if ((s + String.fromCharCode (13)).charCodeAt (i + 1) != 13)
							fdx += spacingH; // + spacingHoriz;
						lastIndex = index;
					}
				}
			}
		}
		m = Utils.iMax (m, fdx);
		return m;
	}
	
	public function drawText (bd: BitmapData, s: String, xpos: Int, ypos: Int)
	{
		var m: Int = 0;
		var index: Int;
		var lastIndex: Int = -1;
		var lastAsc: Int = -1;
		var line: Int = 0;
		var w: Int = textWidth (s, line);
		var fdx: Int = 0;
		if (align == ALIGN_RIGHT) fdx -= w;
		if (align == ALIGN_CENTER) fdx -= (w >> 1);
		var fdy: Int = 0;
		
		for (i in 0...s.length)
		{
			var asc: Int = s.charCodeAt (i);
			if (asc == 13)
			{
				m = Utils.iMax (m, fdx);
				fdx = 0;
				fdy += fontSize + spacingVert + spacingV;
				lastIndex = -1;
				lastAsc = -1;
				line++;
				w = textWidth (s, line);
				fdx = 0;
				if (align == ALIGN_RIGHT) fdx -= w;
				if (align == ALIGN_CENTER) fdx -= (w >> 1);
			}
			else
			{
				index = findChar (asc);
				if ((index >= 0) && (skipChars.indexOf (String.fromCharCode (asc)) == -1))
				{
					if (false && kerning)
					{
						if (lastIndex != -1)
						{
							var k: Int = kerningChars[lastIndex].indexOf (String.fromCharCode (asc));
							if (k >= 0)
								fdx += signedByte (kerningValue[lastIndex].charCodeAt (k) & 0xFF);
						}
					}
					
					var xp: Int = x[index];
					var yp: Int = y[index];
					var ww: Int = width[index];
					var hh: Int = height[index];
					var p: Point = new Point (xpos + fdx - xp + offsetX[index], ypos + fdy - yp + offsetY[index]);
					var r: Rectangle = new Rectangle (xpos + fdx + offsetX[index], ypos + fdy + offsetY[index], ww, hh);
					var matrix: Matrix = new Matrix (1, 0, 0, 1, p.x, p.y);
					bd.draw (abdImages[page[index]], matrix, colorTransform, blendMode,
							new Rectangle (xpos + fdx + offsetX[index], ypos + fdy + offsetY[index], ww, hh), true);
					
					fdx += signedShort (advanceX[index]);
					if ((s + String.fromCharCode (13)).charCodeAt (i + 1) != 13)
						fdx += spacingH; // + spacingHoriz;
					lastIndex = index;
				}
			}
		}
	}
	
	public function findChar (char: Int): Int
	{
		if (char <= 255)
			return ascii[char];
		else
			return idStr.indexOf (String.fromCharCode (char));
	}
	
	public function signedShort (x: Int): Int
	{
		return ((x << 16) >> 16);
	}
	
	public function signedByte (x: Int): Int
	{
		return ((x << 24) >> 24);
	}
	
	public function new (fnt: String, abd: Array<BitmapData>)
	{
		baFnt = Resource.getBytes (fnt).getData ();
		baFnt.endian = Endian.LITTLE_ENDIAN;
		//trace (baFnt.length);
		
#if debug
		var header: Int = baFnt.readUnsignedInt ();
		if ((header & 0xFFFFFF) != 0x464d42)  // "BMF"
			trace ("Invalid file");
#end
		baFnt.position = 3;
		bmfVersion = baFnt.readByte ();
#if debug
		if (bmfVersion != 2 && bmfVersion != 3)
			trace ("Invalid BMF file version: " + bmfVersion);
#end
		var blockType: Int = baFnt.readUnsignedByte ();
		while (blockType != 0)
		{
			var blockLen: Int = baFnt.readUnsignedInt ();
			if (bmfVersion == 2) blockLen -= 4;
			
			//trace (blockType + " " + blockLen);
			
			// first time init
			spacingH = 0;
			spacingV = 0;
			shadow = false;
			shadowX = 0;
			shadowY = 0;
			align = ALIGN_LEFT;
			skipChars = "";
			blendMode = BlendMode.NORMAL;
			colorTransform = new ColorTransform (1, 1, 1, 1, 0, 0, 0, 1);
			
			switch (blockType)
			{
				case 1:
				{
					// info
					fontSize = baFnt.readUnsignedShort ();
					var b: Int = baFnt.readUnsignedByte ();
					bold = (b & 0x80 != 0);
					italic = (b & 0x40 != 0);
					unicode = (b & 0x20 != 0);
					smooth = (b & 0x10 != 0);
					charSet = baFnt.readUnsignedByte ();
					stretchH = baFnt.readUnsignedShort ();
					aa = baFnt.readUnsignedByte ();
					paddingUp = baFnt.readUnsignedByte ();
					paddingRight = baFnt.readUnsignedByte ();
					spacingHoriz = baFnt.readUnsignedByte ();
					spacingVert = baFnt.readUnsignedByte ();
					outline = baFnt.readUnsignedByte ();
					
					baFnt.position += blockLen - 12;
				}
				case 2:
				{
					// common
					lineHeight = baFnt.readUnsignedShort ();
					base = baFnt.readUnsignedShort ();
					scaleX = baFnt.readUnsignedShort ();
					scaleY = baFnt.readUnsignedShort ();
					pages = baFnt.readUnsignedShort ();
					var b: Int = baFnt.readUnsignedByte ();
					packed = (b & 1) != 0;
					if (bmfVersion == 2)
						encoded = (b & 2) != 0;
					else
					{
						alphaChannel = baFnt.readUnsignedByte ();
						redChannel = baFnt.readUnsignedByte ();
						greenChannel = baFnt.readUnsignedByte ();
						blueChannel = baFnt.readUnsignedByte ();
					}
				}
				case 3:
				{
					// load pages
					
					// if (packed) DecodeChannels ();
					
					baFnt.position += blockLen;


/*
  Method LoadPages (Prefix: String, ImageFile: String)
    Local i: Int
    BMFImage = BMFImage[..Pages]
    For i = 0 To Pages - 1
      BMFImage[i] = LoadImage (Prefix + ImageFile.Split (Chr (0))[i], MASKEDIMAGE | FILTEREDIMAGE)
      SetImageHandle (BMFImage[i], 0, 0)
    Next
    If Packed Then
      DecodeChannels ()
    End If
    ImageW = ImageW[..Pages]
    ImageH = ImageH[..Pages]
    For i = 0 To Pages - 1
      ImageW[i] = Pow2Size (BMFImage[i].Width)
      ImageH[i] = Pow2Size (BMFImage[i].Height)
    Next
    Image = BMFImage
    CustomImage = BMFImage
    ShadowImage = BMFImage
    EnableCustomTexture (False)
  End Method

*/
				}
				case 4:
				{
					// load characters
					var n: Int = Math.floor (blockLen / ((bmfVersion == 2)? 18 : 20));
					ascii = new Array ();
					idStr = "";
					id = new Array ();
					x = new Array ();
					y = new Array ();
					width = new Array ();
					height = new Array ();
					offsetX = new Array ();
					offsetY = new Array ();
					advanceX = new Array ();
					page = new Array ();
					for (i in 0 ... n)
					{
						id.push ((bmfVersion == 2)? baFnt.readUnsignedShort () : baFnt.readUnsignedInt ());
						if (id[i] <= 255)
							ascii[id[i]] = i;
						idStr += String.fromCharCode (id[i]);
						x.push (baFnt.readUnsignedShort ());
						y.push (baFnt.readUnsignedShort ());
						width.push (baFnt.readUnsignedShort ());
						height.push (baFnt.readUnsignedShort ());
						offsetX.push (baFnt.readShort ());
						offsetY.push (baFnt.readShort ());
						advanceX.push (baFnt.readShort ());
						page.push (baFnt.readUnsignedByte ());
						var channel: Int = baFnt.readUnsignedByte ();
						if (packed)
						{
							var ch: Int = (channel < 4)? channel - 1 : (channel == 4)? 2 : 3;
							page[i] = 4 * page[i] + ch;
						}
						kerning = false;
						kerningChars = new Array ();
						kerningValue = new Array ();
					}
				}
				case 5:
				{
					// kerning
					var n: Int = Math.floor (blockLen / ((bmfVersion == 2)? 6 : 10));
					for (i in 0 ... n)
					{
						var first: Int = ((bmfVersion == 2)? baFnt.readUnsignedShort () : baFnt.readUnsignedInt ());
						var second: Int = ((bmfVersion == 2)? baFnt.readUnsignedShort () : baFnt.readUnsignedInt ());
						var amount: Int = baFnt.readShort ();
						var j: Int = idStr.indexOf (String.fromCharCode (first));
						if (j >= 0)
						{
							kerningChars[j] += String.fromCharCode (second);
							kerningValue[j] += String.fromCharCode (amount & 0xFF);
							kerning = true;
						}
					}
				}
			}
			if (baFnt.position < baFnt.length)
				blockType = baFnt.readUnsignedByte ();
			else	
				blockType = 0;
		}
		
		abdImages = abd;

		for (i in 0 ... abdImages.length)
		{
			for (y in 0 ... abdImages[i].height)
				for (x in 0 ... abdImages[i].width)
				{
					var rgb: Int = abdImages[i].getPixel32 (x, y);
					var a = Math.floor (((rgb & 0xFF) + ((rgb >> 8) & 0xFF) + ((rgb >> 16) & 0xFF)) / 3);
					//rgb = rgb & 0xFFFFFF;
					rgb = a + (a << 8) + (a << 16);
					abdImages[i].setPixel32 (x, y, (a << 24) + rgb);
				}
		}
		
		//Def.log ("" + idStr.substr(207, 1).charCodeAt(0));
		
	}
	
	
}

