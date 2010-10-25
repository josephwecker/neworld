package gamelib2d;

import flash.display.Bitmap;
import flash.display.BitmapData;
import flash.display.Sprite;
import flash.geom.Matrix;
import flash.geom.Rectangle;
import gamelib2d.BMFont;
import flash.geom.ColorTransform;


class Status
{
	private static var screen: Sprite;
	private static var elements: Array<Status>;
	private static var font: BMFont;
	private static var bmp: Bitmap;
	private static var bd: BitmapData;
	
	public static var METER_MAX: Int = 100;
	
	static var ctShadow: ColorTransform = new ColorTransform (0, 0, 0, 0.125, 0, 0, 0, 0);
	static var ctNormal: ColorTransform = new ColorTransform (1, 1, 1, 1, 0, 0, 0, 0);
	
	public static function initStatusLine (sprScreen: Sprite, x: Int, y: Int, w: Int, h: Int, bmFont: BMFont)
	{
		screen = sprScreen;
		font = bmFont;
		elements = new Array ();
		//bd = new BitmapData (w, h, true, 0x7f000000);
		bd = new BitmapData (w, h, true, 0x00000000);
		bmp = new Bitmap (bd);
		bmp.x = x;
		bmp.y = y;
		screen.addChild (bmp);
	}
	
	public static function runStatusLine ()
	{
		if (elements != null)
			for (e in elements)
				e.run ();
	}
	
	public static function drawStatusLine ()
	{
		if (elements != null)
			for (e in elements)
				e.draw ();
	}
	
	public static function clearStatusLine ()
	{
		//if (elements != null)
		//	for (e in elements)
		//	{
		//		
		//	}
		elements = null;
		if (bmp != null) screen.removeChild (bmp);
		bmp = null;
		bd = null;
	}
	
	public static function setAlpha (a: Float)
	{
		if (bmp != null)
			bmp.alpha = a;
	}
	
	public static function drawBitmapData (bdImage: BitmapData, x: Int, y: Int, ?ct: ColorTransform = null)
	{
		bd.draw (bdImage, new Matrix (1, 0, 0, 1, x, y), ct);
	}
	
	
	public var name: String;
	public var value: Int;
	public var curvalue: Int;
	public var format: String;
	var x: Int;
	var y: Int;
	public var align: Int;
	var lasttext: String;
	var inc: Int;
	var meterw: Int;
	var meterh: Int;
	public var soundFunction: (Void -> Void);
	
	
	public static function setValue (name: String, newValue: Int, ?flush: Bool = false)
	{
		if (elements != null)
			for (e in elements)
				if (e.name == name)
				{
					e.value = newValue;
					if (flush) e.curvalue = newValue;
				}
	}
	
	public static function addValue (name: String, newValue: Int)
	{
		if (elements != null)
			for (e in elements)
				if (e.name == name)
					e.value += newValue;
	}

	public static function getValue (name: String): Int
	{
		var v: Int = 0;
		if (elements != null)
			for (e in elements)
				if (e.name == name)
					v = e.value;
		return v;
	}
	
	
	public function new (name: String, value: Int, format: String, x: Int, y: Int, align: Int, ?inc: Int = 0)
	{
		this.name = name;
		this.value = value;
		this.curvalue = value;
		this.format = format;
		this.x = x;
		this.y = y;
		this.align = align;
		this.inc = inc;
		if (format == "")
		{
			this.meterw = align;
			this.meterh = font.textHeight ("A") >> 1;
			this.y += this.meterh >> 1;
		}
		this.lasttext = "";
		this.soundFunction = null;
		elements.push (this);
	}
	
	public function run ()
	{
		if (curvalue < value)
		{
			curvalue += inc;
			if (curvalue > value)
				curvalue = value;
		}
		if (curvalue > value)
		{
			curvalue -= inc;
			if (curvalue < value)
				curvalue = value;
		}
			
		if (inc == 0) curvalue = value;
		if (format == "") if (curvalue > METER_MAX) curvalue = METER_MAX;
	}
	
	public function draw ()
	{
		if (format == "") // meter
		{
			bd.fillRect (new Rectangle (x - 3, y - 3, meterw + 7, meterh + 7), 0x20000000);
			bd.fillRect (new Rectangle (x - 3, y - 3, meterw + 6, meterh + 6), 0x20000000);
			bd.fillRect (new Rectangle (x - 2, y - 2, meterw + 4, meterh + 4), 0x80000000);
			bd.fillRect (new Rectangle (x - 1, y - 1, meterw + 2, meterh + 2), 0xFFFFFFFF);
			
			bd.fillRect (new Rectangle (x, y, meterw, meterh), 0x80FFFFFF);
			bd.fillRect (new Rectangle (x + 1, y + 1, meterw - 2, meterh - 2), 0x58000000);
			
			if (curvalue > 0)
			{
				var w: Int = Std.int (curvalue * (meterw - 4) / METER_MAX);
				
				var i: Int = Std.int (curvalue * 0x80 / METER_MAX);
				var color: Int = Utils.fWeightBlendRGB (0x60E040, 0xFF4020, curvalue / METER_MAX, 1 - (curvalue / METER_MAX));
				
				bd.fillRect (new Rectangle (x + 2, y + 2, w, meterh - 4), 0xB8000000 + color);
				bd.fillRect (new Rectangle (x + 2, y + 4, w, (meterh >> 1) - 2), 0xF0000000 + color);
			}
		}
		else
		{
		
			var s: String = Utils.formatStr (format, [curvalue]);
			var t: String = s;
			for (i in 0...t.length)
				if (t.charAt (i) == " ")
					s = " " + s;
			if (s != lasttext)
			{
				var ww: Int = font.textWidth (lasttext);
				var xx: Int = x;
				if (align == BMFont.ALIGN_RIGHT) xx -= ww;
				if (align == BMFont.ALIGN_CENTER) xx -= (ww >> 1);
				bd.fillRect (new Rectangle (xx - 1, y - 1, ww + 4, font.textHeight (lasttext) + 4), 0);
				
				font.align = align;
				for (mode in 0...3)
				{
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
								font.drawText (bd, s, x + dx, y + dy);
					font.drawText (bd, s, x + dx, y + dy);
				}
				
				if (soundFunction != null) soundFunction ();
				
				lasttext = s;
			}
		}
	}
	
	public static function iterator ()
	{
		return elements.iterator ();
	}
}
