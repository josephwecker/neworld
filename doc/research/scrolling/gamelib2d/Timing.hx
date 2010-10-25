package gamelib2d;

import flash.Lib;

class Timing
{
	static private var frameRate: Int;
	
	static private var lastMS: Int;
	//static private var sFT: String;
	static private var lasttf: Int;
	static private var lastSec: Int;
	static private var actualFrameCounter: Int;
	static private var logicFrameCounter: Int;
	
	public static function init (?logicFrameRate: Int = 0)
	{
		if (logicFrameRate == 0)
			logicFrameRate = Std.int (Lib.current.stage.frameRate);
		frameRate = logicFrameRate;
		
		actualFrameCounter = 0;
		logicFrameCounter = 0;
		lastMS = Std.int (Date.now ().getTime ()) % 1000;
	}
	
	public static function logicFrames (?max: Int = 5): Int
	{
		actualFrameCounter += 1;

		var t: Int = Std.int (Date.now ().getTime ());
		var ms = Std.int (t) % 1000;
		var sec: Int = Std.int (t / 1000);

		if (ms < lastMS) ms += 1000;

		var tf = Std.int (ms * frameRate / 1000);
		var result = tf - lasttf;
		lasttf = tf;
		
		if (result < 0) result = 0;
		if (result > max) result = max;
		
		//sFT = sFT + "|" + result;

		if (sec != lastSec)
		{
	#if fps
			//haxe.Log.clear ();
			//haxe.Log.setColor (0xFFFFFF);

			trace (logicFrameCounter + " logic");
			trace (actualFrameCounter + " fps"); 
			//trace (sFT);
			trace ("mem: " + flash.system.System.totalMemory / 1024);
	#end
			//sFT = "";
			actualFrameCounter = 0;
			logicFrameCounter = 1;
			lastSec = sec;
		}

		lastMS = ms;
		logicFrameCounter += result;
		
		return result;
	}
	
}

