package gamelib2d;

import flash.net.LocalConnection;

class Utils
{
	public static inline function boolToInt (b: Bool): Int { return b? 1 : 0; }
	public static inline function intToBool (i: Int): Bool { return (i != 0)? true : false; }
	
	public static inline function iMax (i1: Int, i2: Int): Int { return (i1 > i2)? i1 : i2; }
	public static inline function iMin (i1: Int, i2: Int): Int { return (i1 < i2)? i1 : i2; }

	public static inline function rgb (r: Int, g: Int, b: Int): Int { return clipByte (b) + (clipByte (g) << 8) + (clipByte (r) << 16); }
	public static inline function rgba (r: Int, g: Int, b: Int, ?a: Int = 0xFF): Int { return clipByte (b) + (clipByte (g) << 8) + (clipByte (r) << 16) + (clipByte (a) << 24); }

	public static function clipByte (b: Int)
	{
		if (b < 0)
			return 0;
		else
			if (b > 0xFF)
				return 0xFF;
			else
				return b;
	}
	
	public static function blendRGB (rgb1: Int, rgb2: Int)
	{
		return rgb ((((rgb1 >> 16) & 0xFF) + ((rgb2 >> 16) & 0xFF)) >> 1, 
		            (((rgb1 >> 8) & 0xFF) + ((rgb2 >> 8) & 0xFF)) >> 1, 
					 ((rgb1 & 0xFF) + (rgb2 & 0xFF)) >> 1);
	}
	
	public static function addRGB (rgb1: Int, rgb2: Int)
	{
		return rgb (clipByte (((rgb1 >> 16) & 0xFF) + ((rgb2 >> 16) & 0xFF)), 
		            clipByte (((rgb1 >> 8) & 0xFF) + ((rgb2 >> 8) & 0xFF)), 
					clipByte ((rgb1 & 0xFF) + (rgb2 & 0xFF)));
	}
	
	public static function maxRGB (rgb1: Int, rgb2: Int)
	{
		return rgb (iMax (((rgb1 >> 16) & 0xFF), ((rgb2 >> 16) & 0xFF)), 
		            iMax (((rgb1 >> 8) & 0xFF), ((rgb2 >> 8) & 0xFF)), 
					iMax ((rgb1 & 0xFF), (rgb2 & 0xFF)));
	}
	
	public static function fWeightBlendRGB (rgb1: Int, rgb2: Int, f1: Float, f2: Float)
	{
		var r1: Int = (rgb1 >> 16) & 0xFF;
		var r2: Int = (rgb2 >> 16) & 0xFF;
		var g1: Int = (rgb1 >> 8) & 0xFF;
		var g2: Int = (rgb2 >> 8) & 0xFF;
		var b1: Int = (rgb1 & 0xFF);
		var b2: Int = (rgb2 & 0xFF);
		return rgb (Std.int (f1 * r1 + f2 * r2), Std.int (f1 * g1 + f2 * g2), Std.int (f1 * b1 + f2 * b2));
		
	/*
		var w1: Int = Std.int (255 * f1);
		var w2: Int = Std.int (255 * f2);
		return rgb ((w1 * ((rgb1 >> 16) & 0xFF) + w2 * ((rgb2 >> 16) & 0xFF)) >> 9, 
		            (w1 * ((rgb1 >> 8) & 0xFF) + w2 * ((rgb2 >> 8) & 0xFF)) >> 9, 
					(w1 * (rgb1 & 0xFF) + w2 * (rgb2 & 0xFF)) >> 9);
	*/
	}

	public static inline function sgn (x: Int) { if (x == 0) return 0; else if (x > 0) return 1; else return -1; }
	
	public static function xor (b1: Bool, b2: Bool): Bool
	{
		if (b1 && b2)
			return false;
		else
			return (b1 || b2);
	}
	
	public static function safeDiv (x: Float, n: Float): Int
	{
		return Std.int (x / n - ((x < 0)? 1 : 0));
	}

	public static function safeMod (x: Int, y: Int)
	{
		var z: Int = x % y;
		if (z < 0)
			z += y;
		return z;
	}

	public static function iAbs (X: Int)
	{
		if (X < 0) return -X else return X;
	}

	public static function iRnd (n: Int): Int
	{
		return Std.int ((Math.random () * n) - (Math.random () * n));
	}

	public static function rRnd (n: Float): Float  
	{
		return ((Math.random () * n) - (Math.random () * n));
	}

	public static var randSeed1: Int = 0;
	public static var randSeed2: Int = 0;
	
	public static function rnd (?n: Int = 0)
	{
		randSeed1 = (randSeed1 + 0x152 + randSeed2) << 1;
		randSeed2 = (randSeed2 ^ 0x259) + randSeed1;
		randSeed1 = ((randSeed1 << 1) + randSeed2) & 0xFFFF;
		randSeed2 = ((randSeed2 & 0xFF) << 8) + ((randSeed2 & 0xFF00) >> 8);
		if (n == 0)
			return randSeed1;
		else
			return (randSeed1 % n);
	}
	
	public static function randomize ()
	{
		randSeed1 = Date.now ().getDate () + Std.int (Date.now ().getTime () * 0x7FF);
		randSeed2 = Date.now ().getSeconds () * Date.now ().getMinutes () * Date.now ().getHours () + Std.int (Date.now ().getTime () * 0xFF);
	}
	
	public static function gc ()
	{
		// unsupported hack that seems to force a full GC
		try
		{
			var lc1: LocalConnection = new LocalConnection();
			var lc2: LocalConnection = new LocalConnection();

			lc1.connect('name');
			lc2.connect('name');
		}
		catch (e: Dynamic)
		{
		}
	}
	
	public static function formatStr (format: String, values: Array<Dynamic>): String
	{
		var s: String = "";
		var arg: Int = 0;
		while (format != "")
		{
			var c: String = format.charAt (0);
			format = format.substr (1, format.length - 1);
			if (c == "%")
			{
				var fmt: String = "";
				var len: Int = 0;
				var code: Int = -1;
				var c: String = "";
				var base: Int = 10;
				
				while (format != "" && code < 0)
				{
					c = format.charAt (0);
					format = format.substr (1, format.length - 1);
					fmt += c;
					code = "dxXbfs".indexOf (c);
					if (c >= "0" && c <= "9")
						len = 10 * len + "0123456789".indexOf (c);
				}
				fmt = fmt.substr (0, fmt.length - 1);
				var hex = "0123456789abcdef";
				var ss: String = "";
				switch (code)
				{
					case 0: // "d"
					case 1: // "x"
						base = 16;
					case 2: // "X"
						base = 16;
						hex = "0123456789ABCDEF";
					case 3: // "b"
						base = 2;
					case 4: // "f"  // not implemented
						base = 0;
						ss = "" + cast (values[arg++], Float);
					case 5: // "s"
						ss = cast (values[arg++], String);
						base = 0;
				}
				if (base != 0)
				{
					
					var n: Int = cast (values[arg++], Int);
					var nn = n;
					if (nn < 0) nn = -nn;
					while (nn > 0)
					{
						ss = hex.charAt (nn % base) + ss;
						nn = Std.int (nn / base);
					}
					if (ss == "") ss = "0";
					if (n < 0) ss = "-" + ss;
				}
				while (ss.length < len)
					if (fmt.charAt (0) == "0")
						ss = "0" + ss;   // doesn't work correctly for negative numbers
					else
						ss = " " + ss;
				s += ss;
			}
			else
				s += c;
		}
		return s;
	}
	
	
    // sitelock(["www.wieringsoftware.nl"], true);
	public static function sitelock (urls_allowed: Array<String>, allow_local: Bool)
	{
		var lock: Bool = true;
		var domain_parts = flash.Lib.current.loaderInfo.url.split("://");
		if (allow_local && domain_parts[0] == "file")
			lock = false;
		var real_domain = domain_parts[1].split("/");
		for (s in urls_allowed) 
			if (s == real_domain[0]) 
				lock = false;
		if (lock) 
			flash.Lib.current.alpha = 0;
	}

}
