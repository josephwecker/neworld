package gamelib2d;

import flash.events.MouseEvent;
import flash.geom.Rectangle;
import gamelib2d.BMFont;

import flash.events.Event;
import flash.events.KeyboardEvent;
import flash.events.MouseEvent;

import flash.display.Bitmap;
import flash.display.BitmapData;
import flash.display.Sprite;
import flash.geom.ColorTransform;

class Menu
{
	public static inline var KEY_UP     =  flash.ui.Keyboard.UP;
	public static inline var KEY_DOWN   =  flash.ui.Keyboard.DOWN;
	public static inline var KEY_SPACE  =  flash.ui.Keyboard.SPACE;
	public static inline var KEY_ENTER  =  flash.ui.Keyboard.ENTER;
	public static inline var KEY_ESCAPE =  flash.ui.Keyboard.ESCAPE;
	
	public static inline var KEY_W   =  87;
	public static inline var KEY_S   =  83;

	public var active: Bool;
	public var nextAction: Int;
	public var history: Array < Int >;
	
	public var screen: Sprite;
	public var font: BMFont;
	public var centerX: Int;
	public var centerY: Int;
	public var edgeW: Int;
	public var edgeH: Int;
	public var padW: Int;  // left & right space
	public var padH: Int;  // gap between options
	public var backARGB: Int;
	public var selARGB: Int;
	
	public var align: Int;
	
	public var shadowX: Int;
	public var shadowY: Int;
	
	public var bulletLeft: String; 
	public var bulletRight: String; 
	
	public var curMenu: Int;
	public var menuTxt: Array < String >;
	public var menuAction: Array < Int >;
	public var menuParameters: Array < Int >;
	public var selected: Int;
	public var lastSelected: Array < Int >;
	
	public var ctActive: ColorTransform;
	public var ctInactive: ColorTransform;
	public var ctShadow: ColorTransform;
	
	public var initialized: Bool;
	public var menuX: Array < Int >;
	public var menuY: Array < Int >;
	public var menuW: Array < Int >;
	public var menuH: Array < Int >;
	
	public var menuSpr: Sprite;
	public var menuBmp: Bitmap;
	public var menuBD: BitmapData;
	
	public var sndSelect: Int;
	public var sndChoose: Int;
	public var sndBack: Int;
	
	var lastMX: Float;
	var lastMY: Float;
	
	var mouseInMenu: Bool;
	
	var mW: Int;
	var mH: Int;
	
	public function new (s: Sprite)
	{
		screen = s;
		active = false;
	}
	
	public function init (f: BMFont, x: Int, y: Int, ew: Int, eh: Int, pw: Int, bc: Int, sc: Int)
	{
		font = f;
		centerX = x;
		centerY = y;
		edgeW = ew;
		edgeH = eh;
		padW = pw;
		padH = 0;
		backARGB = bc;
		selARGB = sc;
		ctActive = new ColorTransform (1, 1, 1, 1, 0, 0, 0, 1);
		ctInactive = new ColorTransform (0.75, 0.75, 0.75, 1, 0, 0, 0, 1);
		ctShadow = new ColorTransform (0, 0, 0, 1, 0, 0, 0, 1);
		shadowX = 1;
		shadowY = 1;
		nextAction = 1;
		
		bulletLeft = "";  // "> "
		bulletRight = "";  // " <"
		
		align = BMFont.ALIGN_CENTER;
		
		lastSelected = new Array ();
		history = new Array ();
		
		sndSelect = 0;
		sndChoose = 0;
		sndBack = 0;
		mouseInMenu = false;
		
		activate ();
	}
	
	public function activate ()
	{
		active = true;
		flash.Lib.current.stage.addEventListener (MouseEvent.CLICK, onClick);
		flash.Lib.current.stage.addEventListener (KeyboardEvent.KEY_DOWN, onKeyDown);
		flash.Lib.current.stage.addEventListener (KeyboardEvent.KEY_UP, onKeyUp);
	}
	
	public function deactivate ()
	{
		flash.Lib.current.stage.removeEventListener (MouseEvent.CLICK, onClick);
		flash.Lib.current.stage.removeEventListener (KeyboardEvent.KEY_DOWN, onKeyDown);
		flash.Lib.current.stage.removeEventListener (KeyboardEvent.KEY_UP, onKeyUp);
		active = false;
	}
	
	public function newMenu (m: Int)
	{
		if (history.length > 0)
		{
			var l = history.pop ();
			if (l != m)
			{
				history.push (l);
				history.push (m);
			}
		}
		else	
			history.push (m);
		
		clear ();
		
		selected = lastSelected[m];
		active = true;
		
		curMenu = m;
		menuTxt = new Array ();
		menuAction = new Array ();
		menuParameters = new Array ();
		initialized = false;
	}
	
	public function clear ()
	{
		if (active) deactivate ();
		
		if (menuSpr != null) screen.removeChild (menuSpr);
		if (menuBmp != null) menuSpr.removeChild (menuBmp);
		menuSpr = null;
		menuBmp = null;
		if (menuBD != null) menuBD.dispose ();
		menuBD = null;
		
		menuTxt = null;
		menuAction = null;
		menuParameters = null;
		
		menuX = null;
		menuY = null;
		menuW = null;
		menuH = null;
		
		initialized = false;
		active = false;
	}
	
	public function addMenuOption (txt: String, action: Int, parameters: Int)
	{
		menuTxt.push (txt);
		menuAction.push (action);
		menuParameters.push (parameters);
	}
	
	public function draw ()
	{
		if (! active)
			return;
			
		var lastSelected: Int = selected;
		
		if (! initialized)
		{
			mW = 0;
			mH = 0;
			menuX = new Array ();
			menuY = new Array ();
			menuW = new Array ();
			menuH = new Array ();
			for (i in 0...menuTxt.length)
			{
				menuW[i] = font.textWidth (bulletLeft + menuTxt[i] + bulletRight);
				if (menuW[i] > mW)
					mW = menuW[i];
				menuH[i] = font.textHeight (menuTxt[i]);
				menuY[i] = edgeH + mH; 
				mH += menuH[i] + padH;
			}
			mH -= padH;
			for (i in 0...menuTxt.length)
			{
				switch (align)
				{
					case BMFont.ALIGN_LEFT:  	menuX[i] = edgeW + padW + 0;
					case BMFont.ALIGN_CENTER:  	menuX[i] = edgeW + padW + ((mW - menuW[i]) >> 1);
					case BMFont.ALIGN_RIGHT:  	menuX[i] = edgeW + padW + (mW - menuW[i]);
				}
				menuX[i] += font.textWidth (bulletLeft);
			}
			
			menuSpr = new Sprite ();
			menuBD = new BitmapData (mW + 2 * edgeW + 2 * padW, mH + 2 * edgeH, true, 0x00000000);
			menuBmp = new Bitmap (menuBD);
			menuSpr.addChild (menuBmp);
			menuSpr.x = centerX - menuSpr.width / 2;
			menuSpr.y = centerY - menuSpr.height / 2;
			screen.addChild (menuSpr);
			
			nextAction = 0;
			
			activate ();
			initialized = true;
			
			//lastMX = menuSpr.mouseX;
			//lastMY = menuSpr.mouseY;
			lastMX = -1;
			lastMY = -1;
			lastSelected = -1;
		}
		
		var mx: Float = menuSpr.mouseX;
		var my: Float = menuSpr.mouseY;
		if (mx != lastMX || my != lastMY)
		{
			mouseInMenu = false;
			for (i in 0...menuTxt.length)
			{
				if (mx >= edgeW && mx < menuSpr.width - edgeW)
					if (my >= menuY[i] && my < menuY[i] + menuH[i])
					{
						selected = i;
						mouseInMenu = true;
					}
			}
			lastMX = menuSpr.mouseX;
			lastMY = menuSpr.mouseY;
		}
		if (selected != lastSelected) sndSelect++;
		
		menuBD.fillRect (new Rectangle (0, 0, menuBD.width, menuBD.height), backARGB);
		for (i in 0...menuTxt.length)
		{
			font.align = BMFont.ALIGN_LEFT;
			if (ctShadow != null)
			{
				font.colorTransform = ctShadow;
				font.drawText (menuBD, menuTxt[i], menuX[i] + shadowX, menuY[i] + shadowY);
			}
			font.colorTransform = ctInactive;
			var s: String = menuTxt[i];
			var x: Int = menuX[i];
			if (i == selected)
			{
				x -= font.textWidth (bulletLeft);
				s = bulletLeft + s + bulletRight;
				menuBD.fillRect (new Rectangle (edgeW, menuY[i], mW + 2 * padW, menuH[i]), selARGB);
				font.colorTransform = ctActive;
			}
			font.drawText (menuBD, s, x, menuY[i]);
		}
	}
	
	
	function moveUp ()
	{
		if (selected > 0)
			selected--;
		else
			selected = menuTxt.length - 1;
		sndSelect++;
	}
	
	function moveDown ()
	{
		if (selected < menuTxt.length - 1)
			selected++;
		else
			selected = 0;
		sndSelect++;
	}
	
	function onClick (event: MouseEvent)
	{
		if (mouseInMenu)
			if (active && initialized)
				chooseOption ();
	}
	
	function onKeyDown (event: KeyboardEvent)
	{
		var repeat: Bool = Keys.keyIsDown (event.keyCode);
		Keys.setKeyStatus (event.keyCode, true);

		if (active && initialized)
		{
			var k: UInt = event.keyCode;
			
			if (k == KEY_UP || k == KEY_W)
				moveUp ();
			if (k == KEY_DOWN || k == KEY_S)
				moveDown ();
			if (k == KEY_SPACE || k == KEY_ENTER)
				chooseOption ();
			if (k == KEY_ESCAPE)
			{
				if (history.length >= 2)
				{
					lastSelected[curMenu] = selected;
					nextAction = history.pop ();
					nextAction = history.pop ();
					sndBack++;
				}
			}
			
		}
	}
	
	function onKeyUp (event: KeyboardEvent)
	{
		Keys.setKeyStatus (event.keyCode, false);
	}
	
	function chooseOption ()
	{
		nextAction = menuAction[selected];
		lastSelected[curMenu] = selected;
		sndChoose++;
	}
	
	public function getAction (): Int
	{
		var action = nextAction;
		nextAction = 0;
		return action;
	}
	
	public static function selStr (s: String, b: Bool): String
	{
		if (b)
			return "> " + s + " <";
		else
			return s;
	}
	
}

