package gamelib2d;

// splash screen

import flash.display.Bitmap;
import flash.display.BitmapData;
import flash.display.Sprite;
import flash.geom.Matrix;
import flash.events.Event;
import flash.events.KeyboardEvent;
import flash.events.MouseEvent;
import flash.text.TextField;
import flash.ui.Keyboard;
import flash.net.URLRequest;

//
// Project, Properties, Compiler Options, Additional Compiler Options:
//
//   -resource XXX.png@XXX
//
//
// Res.xml (between <frame> ... </frame>:
//
//      <library>
//        <bitmap id="XXX" name="XXX" import="XXX.png"/>
//      </library>
//

class Splash
{
	public var finished: Bool;
	public var screen: Sprite;
	public var stageW: Int;
	public var stageH: Int;
	public var bmp: Bitmap;
	public var timeCounter: Int;
	public var fade: Float;
	
	private var spr: Sprite;
	private var url: String;
	
	public var tf: TextField;
	
	public var FADE_SPEED: Float;
	public var fadePos: Float;
	
	public function new (s: Sprite)
	{
		screen = s;
	}

	public function init (w: Int, h: Int, bd: BitmapData, scaleX: Float, scaleY: Float, bgc: Int, time: Int)
	{
		finished = false;
		stageW = w;
		stageH = h;
		screen.graphics.beginFill (bgc);
		screen.graphics.drawRect (0, 0, stageW, stageH);
		screen.graphics.endFill ();
		bmp = new Bitmap (bd);
		bmp.scaleX = scaleX;
		bmp.scaleY = scaleY;
		bmp.x = (w - bmp.width) / 2;
		bmp.y = (h - bmp.height) / 2;
		bmp.alpha = 0.0;
		FADE_SPEED = 0.05;
		fade = FADE_SPEED;
		fadePos = -fade;
		spr = new Sprite ();
		spr.addChild (bmp);
		screen.addChild (spr);
		timeCounter = time;
		url = "";
		
		flash.Lib.current.stage.addEventListener (MouseEvent.CLICK, onClick);
		flash.Lib.current.stage.addEventListener (KeyboardEvent.KEY_DOWN, onKeyDown);
		flash.Lib.current.stage.addEventListener (KeyboardEvent.KEY_UP, onKeyUp);
	}
	
	public function setURL (link: String)
	{
		url = link;
		spr.buttonMode = true;
	}
	
	public function setupInvisibleTextFieldLink (link: String, dx: Int, y: Int, size: Int, spaces: Int)
	{
		tf = new TextField ();
		tf.width = stageW;
		tf.multiline = true;

		var s: String = "";
		while (s.length < spaces)
			s = s + " ";
		s = "<font face='Helvetica' color='#F0D8B0' size='" + size + "'><b><a href='" + link + "' target='_blank'>" + s + "</a></b></font>";

		tf.htmlText = s;
		tf.selectable = false;
		tf.x = ((stageW - tf.textWidth) / 2) + dx;
		tf.y = y;
		screen.addChild (tf);
		tf.alpha = 0.0;
	}
	
	public function run (): Bool
	{
		if (!finished)
		{
			if (fade > 0)
			{
				if (fadePos < 1.0)
					fadePos += fade;
				else
					fade = 0.0;
			}
			if (fade < 0)
			{
				if (fadePos > 0.0)
					fadePos += fade; 
				else
					finished = true;
			}
			if (fade == 0)
			{
				if (--timeCounter <= 0)
					fade = -FADE_SPEED;
			}
			bmp.alpha = Math.sin (fadePos);
			
			if (finished)
			{
				flash.Lib.current.stage.removeEventListener (MouseEvent.CLICK, onClick);
				flash.Lib.current.stage.removeEventListener (KeyboardEvent.KEY_DOWN, onKeyDown);
				flash.Lib.current.stage.removeEventListener (KeyboardEvent.KEY_UP, onKeyUp);
				if (tf != null)
					screen.removeChild (tf);
				tf = null;
				screen.removeChild (spr);
				spr.removeChild (bmp);
				spr = null;
				bmp = null;
				return false;
			}
			else
				return true;
		}
		else
			return false;
	}

	
	private function onClick (event: MouseEvent)
	{
		if (url != "")
			flash.Lib.getURL (new URLRequest (url), "_blank");
	}
	
	function onKeyDown (event: KeyboardEvent)
	{
		var repeat: Bool = Keys.keyIsDown (event.keyCode);
		Keys.setKeyStatus (event.keyCode, true);
		
		timeCounter = 0;
	}
	
	
	function onKeyUp (event: KeyboardEvent)
	{
		Keys.setKeyStatus (event.keyCode, false);
	}
	
}