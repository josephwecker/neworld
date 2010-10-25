package gamelib2d;

import flash.display.Bitmap;
import flash.display.BitmapData;
import flash.display.Sprite;
import flash.geom.ColorTransform;
import flash.geom.Rectangle;
import gamelib2d.BMFont;
import gamelib2d.Sfxr;

class TextPanel 
{
	static public inline var CR = String.fromCharCode (13);
	var sizeX: Int;
	var sizeY: Int;
	var posX: Int;
	var posY: Int;
	var bd: BitmapData;
	var bmp: Bitmap;
	var curPos: Int;
	var curX: Int;
	var curY: Int;
	var curText: String;
	var nextText: String;
	var textW: Int;
	var nextTextW: Int;
	var activeTime: Int;
	var lastMsg: String;
	var sfxr: Sfxr;
	var font: BMFont;
	
	static var ctShadow: ColorTransform = new ColorTransform (0, 0, 0, 0.25, 0, 0, 0, 0);
	static var ctBlack: ColorTransform = new ColorTransform (0, 0, 0, 1, 0, 0, 0, 0);
	static var ctGrey: ColorTransform = new ColorTransform (0.5, 0.5, 0.5, 1, 0, 0, 0, 0);
	static var ctNormal: ColorTransform = new ColorTransform (1, 1, 1, 1, 0, 0, 0, 0);
	
	public function new (screen: Sprite, bmf: BMFont, szX: Int = 450, szY: Int = 64, xshift: Int = 0, yshift: Int = 0, sound: Bool = true)
	{
		font = bmf;
		//sizeX = Std.int (8 * Def.STAGE_W / 9);
		//sizeY = Std.int (1 * Def.STAGE_H / 5);
		
		sizeX = szX;
		sizeY = szY;
		
		bd = new BitmapData (sizeX, sizeY, true, 0x00000000);
		bmp = new Bitmap (bd);
		
		posX = ((Def.STAGE_W - sizeX) >> 1) + xshift;
		posY = ((Def.STAGE_H - sizeY) - (sizeY >> 2)) + yshift;
		bmp.x = posX;
		bmp.y = posY;
		screen.addChild (bmp);
		
		if (sound)
		{
			sfxr = new Sfxr (0.25, 44100, 16, 0,  -1111, 1111);
			sfxr.create (SfxrCategory.HIT_HURT);
			sfxr.transpose (12);
			sfxr.sound_vol *= 1.25;
			sfxr.p_env_decay /= 8;
			sfxr.mutate ();
			sfxr.generate ();
		}
		else
			sfxr = null;
			
		activeTime = 0;
	}
	
	public function clear ()
	{
		bd = null;
		bmp = null;
		activeTime = 0;
	}
	
	
	// nokia, CENA, flora, meiryo, mspgothic, qamic sans, tex sans
	
	public function createMessage (line1: String, line2: String, time: Int, shadecolor: Int = 0x80000000)
	{
		if (activeTime > 0)
		{
			if (line1 + line2 == lastMsg)
			{
				bmp.alpha = 1.0;
				activeTime = time;
			}
			return;
		}		
		bd.fillRect (new Rectangle (0, 0, sizeX, sizeY), 0x00000000);
		bmp.alpha = 1.0;
		curText = line1;
		nextText = line2;
		curPos = 0;
		font.spacingH = 0;
		textW = font.textWidth (curText);
		nextTextW = font.textWidth (nextText);
		curX = Std.int ((sizeX - textW) / 2);
		curY = 10;
		var w = Utils.iMax (textW, nextTextW) + 40;
		var h = sizeY;
		if (nextText == "")
		{
			curY += 24; 
			h = (h >> 1) + curY;
		}
		bd.fillRect (new Rectangle ((sizeX - w) >> 1, curY - 10, w, h), shadecolor);
		
		activeTime = time;
		lastMsg = line1 + line2;
	}
	
	
	public function run ()
	{
		if (activeTime > 0)
		{
			if (curText + nextText == "")
			{
				activeTime--;
				if (activeTime < 50)
					bmp.alpha = activeTime / 50;
			}
		}
	}
	
	public function draw ()
	{
		if (activeTime < 0)		
			return;
		if (activeTime == 0)
		{
			bd.fillRect (new Rectangle (0, 0, sizeX, sizeY), 0);
			return;
		}
			
		var letterTime: Int = 3;
		var charPos = Std.int (curPos / letterTime);
		if (charPos >= curText.length)
		{
			if (nextText != "")
			{
				curText = nextText;
				nextText = "";
				curX = (sizeX - nextTextW) >> 1;
				curY = sizeY >> 1;
				curPos = 0;
				charPos = 0;
			}
			else
			{
				curText = "";
				return; 
			}
		}
		var mode = curPos % letterTime;
		var letter = curText.substr (charPos, 1);
		var dx: Int = 0;
		var dy: Int = 0;
		switch (mode)
		{
			case 0:
				{
					font.colorTransform = ctShadow;
					dx = 3;
					dy = 3;
				}
			case 1: font.colorTransform = ctShadow;
			case 2: font.colorTransform = ctNormal;
		}
		if (mode == 1)
			for (dx in -1...3)
				for (dy in -1...3)
					font.drawText (bd, letter, curX + dx, curY + dy);
		font.drawText (bd, letter, curX + dx, curY + dy);
		
		font.colorTransform = ctNormal;
		font.drawText (bd, letter, curX, curY);
		if (mode == letterTime - 1)
		{
			var w = font.textWidth (letter);
			var h = font.textHeight (letter);
			curX += w + font.spacingH;
			if (sfxr != null) 
				if (charPos % 2 == 0) 
					sfxr.play ();
		}
		curPos++;
	}
	
}
