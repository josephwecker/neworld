package gamelib2d;

import flash.display.Sprite;
import flash.text.TextField;
import gamelib2d.Utils;

typedef DirectionSet =
{
	var top: Bool;
	var left: Bool;
	var bottom: Bool;
	var right: Bool;
}

class Def
{
#if debug
	public static var tfDebug: TextField = new TextField ();
#end
	
	public static var STAGE_W: Int = 640;
	public static var STAGE_H: Int = 480;

	public static var globalTimeCounter: Int = 0; 
	public static var globalFrameCounter: Int = 0;  
	
	public inline static var UPPER_BOUND  =  1 << 0;
	public inline static var LEFT_BOUND   =  1 << 1;
	public inline static var LOWER_BOUND  =  1 << 2;
	public inline static var RIGHT_BOUND  =  1 << 3;

	public static function directionSetToInt (ds: DirectionSet): Int
	{
		return  (Utils.boolToInt (ds.top) * UPPER_BOUND) +
				(Utils.boolToInt (ds.left) * LEFT_BOUND) + 
				(Utils.boolToInt (ds.right) * RIGHT_BOUND) +
				(Utils.boolToInt (ds.bottom) * LOWER_BOUND);
	}

	public static function intToDirectionSet (i: Int): DirectionSet 
	{
		return 
		{
			top: Utils.intToBool (i & UPPER_BOUND),
			left: Utils.intToBool (i & LEFT_BOUND),
			right: Utils.intToBool (i & RIGHT_BOUND),
			bottom: Utils.intToBool (i & LOWER_BOUND) 
		};
	}

	public inline static var  TF_UPSIDEDOWN  =  1 << 29;
	public inline static var  TF_MIRROR      =  1 << 30;
	public inline static var  TF_SEQUENCE    =  1 << 31;

	public inline static var  TF_FLAGS       =  TF_SEQUENCE | TF_MIRROR | TF_UPSIDEDOWN;

	public inline static var  GRP_DIAG_BOUNDS    =  1 << 29;
	public inline static var  GRP_MAP_CODES      =  1 << 30;
	public inline static var  GRP_STATIC_BOUNDS  =  1 << 31;


	public inline static var  BIGINT  =  0x07FFFFFF;
	
#if debug
	public static function log (text)
	{
		tfDebug.htmlText += text + " ";
		//tfDebug.htmlText = "<font face='Helvetica' color='#FFFFFF' size='12'><b>" + text;
	}

	public static function initLog (screen: Sprite, width: Int)
	{
		tfDebug.width = width - 20;
		tfDebug.wordWrap = true;
		tfDebug.htmlText = "<font face='Helvetica' color='#FFFFFF' size='12'><b>";
		tfDebug.x = 10; // (Def.STAGE_W - tfDebug.textWidth) / 2;
		tfDebug.y = 10; // Def.STAGE_H / 5 - tfDebug.textHeight / 2;
		tfDebug.mouseEnabled = false;
		screen.addChild (tfDebug);
	}
#end

}
