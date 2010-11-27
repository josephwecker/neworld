package gamelib2d;

import flash.geom.Matrix;
import flash.geom.Transform;
import gamelib2d.Def;
import gamelib2d.TileSet;
import gamelib2d.Layer;
import haxe.Log;

#if flash8
	import flash.MovieClip;
	import flash.Key;
#elseif flash9
	import flash.display.MovieClip;
	import flash.display.Sprite;
	import flash.display.Bitmap;
	import flash.display.BlendMode;
#end
import flash.display.BitmapData;
import flash.geom.Point;
import flash.geom.Rectangle;
import flash.geom.ColorTransform;


enum MsgTarget
{
	mtAll;
	mtSelf;
	mtPar (i: Int);
	mtObj (o: Obj);
	mtCode (i: Int);
	mtCodeC (i: Int, mt: MsgTarget);
	mtName (s: String);
	mtNameC (s: String, mt: MsgTarget);
	mtGroup (i: Int);
	mtGroupC (i: Int, mt: MsgTarget);
	
}

typedef Msg =
{
	var msg: String;
	var par1: Float;
	var par2: Float;
	var source: Obj;
	var target: MsgTarget;
	var time: Int;
}


typedef FocusInfo =
{
	var objInFocus: Obj;
	var scrollUp: Float;
	var scrollLeft: Float;
	var scrollRight: Float;
	var scrollDown: Float;
	var maxXV: Float;
	var maxYV: Float;
	var moveScreen: Bool;
}


typedef Collision = 
{
	var obj1: Obj;
	var obj2: Obj;
	var bounds: DirectionSet;
}


typedef ObjectFlags =
{
	autoMirrorLeftRight: Bool,
	autoMirrorUpDown: Bool,
	autoMirrorOffsetX: Bool,
	autoMirrorOffsetY: Bool,
	frameBasedAnimation: Bool,
	onlyApearOffScreen: Bool,
	staticObject: Bool,
	allowKeyboardRepeat: Bool,
	killAfterAnimation: Bool,
	ignoreLevelBounds: Bool
}


class Obj
{
	static inline var MARGIN = 0.0001;
	
	static inline var FRACTION = 0.0000000001;

	public static var objlist: List<Obj> = new List ();
	public static var msglist: List<Msg> = new List ();

	public static var focus: FocusInfo = null;

	#if flash8
		private var mcContainer: MovieClip;
		private var bitmap: MovieClip;
		static var id = 1;
	#elseif flash9
		private var mcContainer: Sprite;
		public var bitmap: Bitmap;
	#end

	private var surface: BitmapData;
	private var logicFrame: Int;

	public var active: Bool;
	public var ignore: Bool;
	public var objName: String;
	public var ts: TileSet;
	public var layer: Layer;
	public var tag: Int;
	public var animation: Int;
	public var animFlags: Int;
	public var x: Float;
	public var y: Float;
	public var sizeX: Int;
	public var sizeY: Int;
	public var origXPos: Float;
	public var origYPos: Float;
	public var speedX: Float;
	public var speedY: Float;
	public var curHDir: Int;
	public var curVDir: Int;
	public var lastHDir: Int;
	public var lastVDir: Int;
	public var group: Int;
	public var bounceGroup: Int;
	public var collideGroup: Int;
	public var accX: Float;
	public var accY: Float;
	public var slowDownX: Float;
	public var slowDownY: Float;
	public var maxSpeedX: Float;
	public var maxSpeedY: Float;
	public var bounceX: Float;
	public var bounceY: Float;
	public var shiftX: Int;
	public var shiftY: Int;
	public var alpha: Float;
	public var timeCounter: Int;
	public var animationTimeCounter: Int;
	public var timeToLive: Int;
	public var framesToLive: Int;
	public var blinkTime: Int;
	public var waitTime: Int;
	public var moveDelay: Int;
	public var skipFrameEvent: Int;
	public var offsetX: Int;
	public var offsetY: Int;
	public var levelBounds: DirectionSet;
	// public var BounceSequences: Array<Int>;
	public var collideTiles: Array<Int>;
	public var mapX: Int;
	public var mapY: Int;
	public var tileToPutBack: Int;
	public var objCode: Int;
	public var objPar: Int;
	
	private var lastSpeedX: Float;
	private var lastSpeedY: Float;

	private var lastTile: Int;
	private var lastFlags: Int;
	private var alreadyBounced: Int;

	public var forgetX: Int;
	public var forgetY: Int;

	public var spriteScaleX: Float;
	public var spriteScaleY: Float;

	public var useKeyboard: Bool;
	public var useMouse: Bool;
	public var lastMouseOver: Bool;

	public var lastDirectionUp: Bool;
	public var lastDirectionLeft: Bool;
	public var mirrorLeftRight: Bool;
	public var mirrorUpDown: Bool;
	
	public var objFlags: ObjectFlags;
	public var simple: Bool;

	public var collisions: Array<Collision>;
	public var ownObjList: List<Obj>;
	
	public var ct: ColorTransform;
	public var redrawBitmap: Bool;
	
	public var checkDiag: Bool;
	public var slideSpeed: Float;
	
	public var diag: Int;
	private var diagX: Int;
	private var diagY: Int;
	private var diagRC: Int;
	private var diagX1: Int;
	private var diagY1: Int;
	private var diagX2: Int;
	private var diagY2: Int;
	
	


	public function new (mc)
	{
		mcContainer = mc;
		objlist.add (this);
	}


	public static function clear ()
	{
		for (obj in objlist)
		{
			if (obj != null)
			{
				obj.active = false;
				obj.deleteObj ();
				if (obj.layer != null) { obj.layer.clear (); obj.layer = null; }
				if (obj.ts != null) { obj.ts.clear (); obj.ts = null; }
			}
			obj = null;
		}
		focus = null;
		msglist = new List ();
		objlist = new List ();
	}


	public function initStatic (name: String, code: Int, par: Int, xp: Float, yp: Float, tileset: TileSet, anim: Int, animflags: Int, ?lyr: Layer)
	{
		init (name, code, par, -1, -1, xp, yp, tileset, anim, animflags, lyr);
		objFlags.staticObject = true;
	}

	
	public static var defaultObjFlags: ObjectFlags =
	{
		autoMirrorLeftRight: false,
		autoMirrorUpDown: false,
		autoMirrorOffsetX: false,
		autoMirrorOffsetY: false,
		frameBasedAnimation: false,
		onlyApearOffScreen: false,
		staticObject: false,
		allowKeyboardRepeat: false,
		killAfterAnimation: false,
		ignoreLevelBounds: false
	}
	

	public function init (name: String, code: Int, par: Int, mapPosX: Int, mapPosY: Int, xp: Float, yp: Float, tileset: TileSet,
							anim: Int, animflags: Int, ?lyr: Layer, ?objflags: ObjectFlags)
	{
		active = true;
		ignore = false;
		objName = name;
		ts = tileset;
		animation = anim;
		animFlags = animflags;
		mapX = mapPosX;
		mapY = mapPosY;
		layer = lyr;
		spriteScaleX = 1.0;
		spriteScaleY = 1.0;
		if (layer == null)
		{
			x = mapPosX * ts.tileW / 2;
			y = mapPosY * ts.tileH / 2;
		}
		else
		{
			x = mapPosX * layer.ts.tileW + layer.ts.tileW / 2 - ts.tileW / 2;
			y = mapPosY * layer.ts.tileH + layer.ts.tileH / 2 - ts.tileH / 2;
		}
		if (mapPosX < 0) x = xp else x += xp;
		if (mapPosY < 0) y = yp else y += yp;

		if (objflags == null)
			objFlags = 
			{
				autoMirrorLeftRight: false,
				autoMirrorUpDown: false,
				autoMirrorOffsetX: false,
				autoMirrorOffsetY: false,
				frameBasedAnimation: false,
				onlyApearOffScreen: false,
				staticObject: false,
				allowKeyboardRepeat: false,
				killAfterAnimation: false,
				ignoreLevelBounds: false
			}
		else
			objFlags = objflags;
	
		tag = 0;
			
		sizeX = ts.tileW;
		sizeY = ts.tileH;
		speedX = 0;
		speedY = 0;
		curHDir = 0;
		curVDir = 0;
		lastHDir = 0;
		lastVDir = 0;
		group = 0;
		bounceGroup = 0;
		collideGroup = 0;
		accX = 0;
		accY = 0;
		slowDownX = 0;
		slowDownY = 0;
		shiftX = 0;
		shiftY = 0;
		bounceX = 0;
		bounceY = 0;
		maxSpeedX = 0;
		maxSpeedY = 0;
		timeCounter = 0;

		useKeyboard = false;
		useMouse = false;
		lastMouseOver = false;

		animationTimeCounter = Def.BIGINT;
		timeToLive = 0;
		framesToLive = 0;
		blinkTime = 0;
		waitTime = 0;
		moveDelay = 0;
		skipFrameEvent = 0;

		offsetX = 0;
		offsetY = 0;
		levelBounds = { top: true, left: true, right: true, bottom: true };
		collideTiles = new Array ();

		tileToPutBack = -1;  // changed from 0
		objCode = code;
		objPar = par;

		alpha = 1.0;

		lastTile = -1;
		lastFlags = -1;

		logicFrame = 0;
		forgetX = 0;
		forgetY = 0;

		surface = new BitmapData (ts.tileW, ts.tileH, true, 0x0);

		#if flash8
			var depth = flash.Lib._root.getNextHighestDepth ();
			bitmap = mcContainer.createEmptyMovieClip ("ID" + id++, depth);
			bitmap._visible = false;
			setSpriteScale (spriteScaleX, spriteScaleY);
			bitmap.cacheAsBitmap = true;
			bitmap.attachBitmap (surface, depth);
		#elseif flash9
			bitmap = new Bitmap (surface);
			bitmap.visible = false;
			setSpriteScale (spriteScaleX, spriteScaleY);
			bitmap.cacheAsBitmap = true;
			mcContainer.addChild (bitmap);
		#end

		mirrorLeftRight = false;
		mirrorUpDown = false;
		lastDirectionUp = false;
		lastDirectionLeft = false;
		
		simple = false;
		
		checkDiag = false;
		slideSpeed = 0.0;
		diag = 0;
		
		collisions = new Array ();
		ownObjList = null;
		
		ct = null;
		redrawBitmap = false;
		
		onCreate ();
	}


	public function setSpriteScale (xscale: Float, yscale: Float)
	{
		var xs: Float = 1.0;
		var ys: Float = 1.0; 
		if (layer != null)
		{
			xs = layer.scaleX;
			ys = layer.scaleY;
		}

		spriteScaleX = xscale;
		spriteScaleY = yscale;

		#if flash8
			bitmap._xscale = 100 * spriteScaleX * xs;
			bitmap._yscale = 100 * spriteScaleY * ys;
		#elseif flash9
			bitmap.scaleX = spriteScaleX * xs;
			bitmap.scaleY = spriteScaleY * ys;
		#end
	}


	public static function clearMessages ()
	{
		msglist = new List ();
	}
  
	public function broadcastMessage (msg: String, p1: Float, p2: Float, t: MsgTarget, time: Int)
	{
		msglist.add ({ msg: msg, par1: p1, par2: p2, source: this, target: t, time: time });
	}

	public static function broadcastMessageStatic (msg: String, p1: Float, p2: Float, t: MsgTarget, time: Int)
	{
		msglist.add ({ msg: msg, par1: p1, par2: p2, source: null, target: t, time: time });
	}


	function checkMsgTarget (mt: MsgTarget, src: Obj): Bool
	{
		return switch (mt)
		{
			case mtAll: 			true;
			case mtSelf: 			src == this;
			case mtObj (o):         this == o;
			case mtPar (i): 		objPar == i;
			case mtCode (i): 		(objCode == i);
			case mtCodeC (i, t):	(objCode == i) && checkMsgTarget (t, src);
			case mtName (s): 		(objName == s);
			case mtNameC (s, t):	(objName == s) && checkMsgTarget (t, src);
			case mtGroup (i): 		(group & i != 0);
			case mtGroupC (i, t):	(group & i != 0) && checkMsgTarget (t, src);
		}
	}

	
	public static function runMessages ()
	{
		for (msg in msglist)
		{
			if (--msg.time <= 0)
			{
				var found: Bool = false;
				for (obj in objlist)
					if (obj.active)
						if (obj.checkMsgTarget (msg.target, msg.source))
						{
							obj.onMessage (msg.msg, msg.par1, msg.par2);
							if (msglist.isEmpty ()) return;
							found = true;
						}
				
				//if (! found) trace ("target not found!");
				
				msglist.remove (msg);
			}	
		}
	}


	public function onCreate ()
	{
		// override this
	}

	public function onMessage (msg: String, par1: Float, par2: Float)
	{
		// override this
	}

	public function onAppear ()
	{
		// override this
	}

	public function onDisappear ()
	{
		// override this
	}

	public function onKeyDown (key: UInt)
	{
		// override this
	}

	public function onKeyUp (key: UInt)
	{
		// override this
	}

	public function onAnimation ()
	{
		// override this
	}

	public function onBounce (ds: DirectionSet, ?obj: Obj)
	{
		// override this
	}

	public function onFrame ()
	{
		// override this
	}

	public function onCollide (obj: Obj, ds: DirectionSet)
	{
		// override this
	}

	public function onCollideTile (x: Int, y: Int, tile: Int, ds: DirectionSet): Int
	{
		// override this
		return tile;
	}

	public function onKill ()
	{
		// override this
	}

	public function onDestroy ()
	{
		// override this
	}

	public function onNewDirection (Horz: Bool, Vert: Bool)
	{
		// override this
	}

	public function onSqueeze ()
	{
		// override this
	}

	public function onMouseOver ()
	{
		// override this
	}

	public function onMouseOut ()
	{
		// override this
	}

	public function onClick ()
	{
		// override this
	}

	
	public function checkMouse (X: Float, Y: Float): Bool
	{
		// override this
		return false;
	}


	public function setBlendMode (bm)  // BlendMode
	{
		#if flash9
			bitmap.blendMode = bm;
		#elseif flash8
			bitmap.blendMode = bm;
		#end
	}


	public function createObjectFocus (scrollup: Float, scrollleft: Float, scrollright: Float, scrolldown: Float, 
										maxxv: Float, maxyv: Float, movescreen: Bool)
	{
		focus = { objInFocus: this, 
				  scrollUp: scrollup, 
				  scrollLeft: scrollleft, 
				  scrollRight: scrollright, 
				  scrollDown: scrolldown, 
				  maxXV: maxxv, 
				  maxYV: maxyv, 
				  moveScreen: movescreen };
	}


	public function clearObjectFocus ()
	{
		focus = null;
	}


	//public static function checkObjectFocus (xview: Float, yview: Float): { viewX: Float, viewY: Float }
	public static function checkObjectFocus (xview: Float, yview: Float): Point
	{
		if (focus != null)
		{
			var obj: Obj = focus.objInFocus;
			if (obj != null)
			{
				var xv: Float = xview;
				var yv: Float = yview;
				var xsc: Float = 1.0;
				var ysc: Float = 1.0;
				if (obj.layer != null)
				{
					xsc = obj.layer.scaleX;
					ysc = obj.layer.scaleY;
				}

				#if flash9
					var screenw: Float = Def.STAGE_W;
					var screenh: Float = Def.STAGE_H;
				#elseif flash8
					var screenw: Float = Def.STAGE_W;
					var screenh: Float = Def.STAGE_H;
				#end

				var xp = xsc * (obj.x + obj.sizeX / 2);
				var yp = ysc * (obj.y + obj.sizeY / 2);
				
				//if (obj.x > obj.origXPos)
				if (xp + focus.scrollRight > xv + screenw) xv = xp + focus.scrollRight - screenw;
				//if (obj.x < obj.origXPos)
				if (xp - focus.scrollLeft < xv) xv = xp - focus.scrollLeft;
				//if (obj.y > obj.origYPos)
				if (yp + focus.scrollDown > yv + screenh) yv = yp + focus.scrollDown - screenh;
				//if (obj.y < obj.origYPos)
				if (yp - focus.scrollUp < yv) yv = yp - focus.scrollUp;

				var dx: Float = xv - xview;
				var dy: Float = yv - yview;
				if (! focus.moveScreen)
				{
					if (dx > focus.maxXV) dx = focus.maxXV;
					if (dy > focus.maxYV) dy = focus.maxYV;
					if (dx < -focus.maxXV) dx = -focus.maxXV;
					if (dy < -focus.maxYV) dy = -focus.maxYV;
				}
				xview = xview + dx;
				yview = yview + dy;
				focus.moveScreen = false;

				var layerw: Float = screenw;
				var layerh: Float = screenh;

				if (obj.layer != null)
				{
					//layerw = obj.layer.mapW * obj.layer.ts.tileW * xsc;
					//layerh = obj.layer.mapH * obj.layer.ts.tileH * ysc;
					layerw = obj.layer.width ();
					layerh = obj.layer.height ();
				}

				if (xview + screenw > layerw) xview = layerw - screenw;
				if (yview + screenh > layerh) yview = layerh - screenh;
				if (xview < 0) xview = 0;
				if (yview < 0) yview = 0;
			}
		}
		return new Point (xview, yview);
	}


	public function changeDepth (newMC)
	{
		#if flash9
			mcContainer.removeChild (bitmap);
			mcContainer = newMC;
			mcContainer.addChild (bitmap);
		#elseif flash8
			bitmap.swapDepths (newMC);
		#end
	}


	public function checkDirection ()
	{
		if (active && (! simple))
		{
			lastHDir = curHDir;
			lastVDir = curVDir;
			curHDir = 0;
			if (x < origXPos - MARGIN) curHDir = -1;
			if (x > origXPos + MARGIN) curHDir =  1;

			curVDir = 0;
			if (y < origYPos - MARGIN) curVDir = -1;
			if (y > origYPos + MARGIN) curVDir =  1;
			
			onNewDirection (curHDir != lastHDir, curVDir != lastVDir);
			
			if (curHDir == -1) lastDirectionLeft = true;
			if (curHDir ==  1) lastDirectionLeft = false;
			if (curVDir == -1) lastDirectionUp = true;
			if (curVDir ==  1) lastDirectionUp = false;
		}
	}


	public function touchTile (mapx: Int, mapy: Int, bounds: Int)
	{
		var tile: Int;
		var newTile: Int;

		if (layer != null)
		{
			layer.curMapX = mapx;
			layer.curMapY = mapy;
			layer.curFlags = 0;
			if ((collideTiles.length > 0) && layer.findCurPosInMap ())
			{
				tile = layer.mapData[layer.curMapY][layer.curMapX];
				for (i in 0...collideTiles.length)
					if (collideTiles[i] == tile)
					{
						newTile = onCollideTile (layer.curMapX, layer.curMapY, tile, Def.intToDirectionSet (bounds));
						if (newTile != tile)
							//layer.mapData[layer.curMapY][layer.curMapX] = newTile;
							layer.writeMap (layer.curMapX, layer.curMapY, newTile);
					}
			}
		}
	}


	public function bounce (ds: DirectionSet, freeSpaceX: Float, freeSpaceY: Float, ?obj: Obj)
	{
		var xb: Float = bounceX;
		var yb: Float = bounceY;
		if (xb < 0) xb = Math.abs (xb * speedX);
		if (yb < 0) yb = Math.abs (yb * speedY);

		onBounce (ds, obj);

		if (ds.top)     { speedY = -yb;  y = origYPos + freeSpaceY; }
		if (ds.left)    { speedX = -xb;  x = origXPos + freeSpaceX; }
		if (ds.bottom)  { speedY =  yb;  y = origYPos - freeSpaceY; }
		if (ds.right)   { speedX =  xb;  x = origXPos - freeSpaceX; }

		alreadyBounced |= Def.directionSetToInt (ds);
	}

	
	public var debugs: String;
	
	public function checkLevelBounds ()
	{
		if ((layer != null) && (! simple))
		{
			var i, j;
			var W: Int = layer.ts.tileW;
			var H: Int = layer.ts.tileH;
			var oldXPos1: Float = x;
			var oldXPos2: Float = oldXPos1 + sizeX - FRACTION;
			var oldYPos1: Float = y;
			var oldYPos2: Float = oldYPos1 + sizeY - FRACTION;
			var XPos1: Float = oldXPos1 + speedX;
			var XPos2: Float = oldXPos2 + speedX;
			var YPos1: Float = oldYPos1 + speedY;
			var YPos2: Float = oldYPos2 + speedY;
			var oldMapX1 = Utils.safeDiv (oldXPos1, W); // Std.int (oldXPos1 / W);
			var oldMapX2 = Utils.safeDiv (oldXPos2, W); // Std.int (oldXPos2 / W);
			var oldMapY1 = Utils.safeDiv (oldYPos1, H); // Std.int (oldYPos1 / H);
			var oldMapY2 = Utils.safeDiv (oldYPos2, H); // Std.int (oldYPos2 / H);
			var MapX1 = Utils.safeDiv (XPos1, W); // Std.int (XPos1 / W);
			var MapX2 = Utils.safeDiv (XPos2, W); // Std.int (XPos2 / W);
			var MapY1 = Utils.safeDiv (YPos1, H); // Std.int (YPos1 / H);
			var MapY2 = Utils.safeDiv (YPos2, H); // Std.int (YPos2 / H);
			var XBlock = false;
			var YBlock = false;
			var NewX = 0; // undefined
			var NewY = 0; // undefined
			var XBnd = 0;
			var YBnd = 0;
			var FreeSpaceX: Float = 0.0;
			var FreeSpaceY: Float = 0.0;
			
			lastSpeedX = speedX;
			lastSpeedY = speedY;

			if (MapX1 < oldMapX1)
			{
				XBlock = true;
				NewX = MapX1;
				XBnd = Def.RIGHT_BOUND;
				FreeSpaceX = (oldXPos1 - oldMapX1 * W);
			}
			if (MapX2 > oldMapX2)
			{
				XBlock = true;
				NewX = MapX2;
				XBnd = Def.LEFT_BOUND;
				//FreeSpaceX = MapX2 * W - oldXPos2 - MARGIN;
				FreeSpaceX = W - 1 - (Std.int (oldXPos2) % W);
			}
			if (MapY1 < oldMapY1)
			{
				YBlock = true;
				NewY = MapY1;
				YBnd = Def.LOWER_BOUND;
				FreeSpaceY = (oldYPos1 - oldMapY1 * H);
			}
			if (MapY2 > oldMapY2)
			{
				YBlock = true;
				NewY = MapY2;
				YBnd = Def.UPPER_BOUND;
				//FreeSpaceY = MapY2 * H - oldYPos2 - MARGIN;
				FreeSpaceY = H - 1 - (Std.int (oldYPos2) % H);
			}

			var XBump: Bool = false;
			var YBump: Bool = false;
			var Bound = 0;
			var EdgeBnd = Def.directionSetToInt (levelBounds);   // ' 0xFFF0 | levelBounds;

			if (objFlags.ignoreLevelBounds) EdgeBnd = 0;
			
			if (XBlock)
			{
				for (j in oldMapY1...oldMapY2 + 1)
				{
					Bound |= (layer.readBoundMap (NewX, j, EdgeBnd, bounceGroup, 0x0F) & XBnd);
					touchTile (NewX, j, XBnd);
				}
				if (Bound & XBnd != 0) XBump = true;
			}
			if (YBlock)
			{
				for (i in oldMapX1...oldMapX2 + 1)
				{
					Bound |= (layer.readBoundMap (i, NewY, EdgeBnd, bounceGroup, 0x0F) & YBnd);
					touchTile (i, NewY, YBnd);
				}
				if (Bound & YBnd != 0) YBump = true;
			}

			if ((XBlock && YBlock) && ((!XBump) && (!YBump)))
			{
				Bound |= (layer.readBoundMap (NewX, NewY, EdgeBnd, bounceGroup, 0x0F) & (XBnd | YBnd));
				touchTile (NewX, NewY, XBnd | YBnd);
				if (Bound & XBnd != 0) XBump = true;
				if (Bound & YBnd != 0) YBump = true;

				if (XBump && YBump)
				{
					if (Math.abs (speedX) > Math.abs (speedY))
					{
						XBump = false;
						Bound = Bound & ~XBnd;
					}
					else
					{
						YBump = false;
						Bound = Bound & ~YBnd;
					}
				}
			}
			if (!XBump) FreeSpaceX = 0;
			if (!YBump) FreeSpaceY = 0;
			if (Bound != 0) bounce (Def.intToDirectionSet (Bound), FreeSpaceX, FreeSpaceY, null);

			
			// diag
			if (checkDiag)
			{
				if (speedY < -1.0)
					diag = 0;
				else
					if (diag == 0)
					{
						var curX: Float = x + sizeX / 2;
						var curY: Float = y + sizeY - 1;
						var newX: Float = curX + speedX;
						var newY: Float = curY + speedY;
						var curMapX: Int = Utils.safeDiv (curX, W);
						var curMapY: Int = Utils.safeDiv (curY, H);
						var newMapX: Int = Utils.safeDiv (newX, W);
						var newMapY: Int = Utils.safeDiv (newY, H);
						
						var bnd: Int = 0;
						var dgbnd: Int = 0;
						var bndx: Int = 0;
						var bndy: Int = 0;
						var l: Int = curMapX;
						var r: Int = newMapX;
						if (l > r)
						{
							l = newMapX;
							r = curMapX;
						}
						bnd = 0;
						for (dgx in l ... r + 1)
							for (dgy in curMapY ... newMapY + 2)
							{
								dgbnd = (layer.readBoundMap (dgx, dgy, EdgeBnd, bounceGroup, 0x0)) >> 16;
								if (dgbnd & 0x80 != 0)
								{
									bndx = dgx;
									bndy = dgy;
									bnd = dgbnd;
								}
							}
						if (bnd != 0)
						{
							var i: Int;
							diagX = bndx * W + (W >> 1);
							diagY = bndy * H + (H >> 1);
							if (bnd & 1 == 1)
								diagRC = -1;  // "\"
							else
								diagRC =  1;  // "/"
							i= 0;
							while (layer.readBoundMap (bndx + i, bndy - diagRC * i, EdgeBnd, bounceGroup, 0x0) >> 16 == bnd)
								i--;
							diagX1 = (bndx + i + 1) * W  ;// - (W >> 1);
							diagY1 = (bndy - diagRC * i + 1) * H;
							i= 0;
							while (layer.readBoundMap (bndx + i, bndy - diagRC * i, EdgeBnd, bounceGroup, 0x0) >> 16 == bnd)
								i++;
							diagX2 = (bndx + i - 1) * W  +W;// + (W >> 1);
							diagY2 = (bndy - diagRC * (i - 1) + 1) * H;
							diag = diagRC;
						}
					}
				
				if (diag != 0)
				{
					var curX: Float = x + sizeX / 2;
					var curY: Float = y + sizeY - 1;
					var newX: Float = curX + speedX;
					var newY: Float = curY + speedY;
					
					FreeSpaceX = 0;
					FreeSpaceY = 0;
					//var i: Int = diagY - Utils.safeDiv (diagRC * H * (curX - diagX), W) - 1 * Utils.boolToInt (diagRC < 0);
					var i: Int = diagY;
					var j: Int = Std.int (curX);
					if (j < diagX1)
						j = diagX1;
					if (j > diagX2 - 1)
						j = diagX2 - 1;
					i -= Utils.safeDiv (diagRC * H * (j - diagX), W) + 1; //  - 1 * Utils.boolToInt (diagRC < 0);
  					
					//if (i <= newY)
					{
						//y = i - (sizeY - 1);
						//origYPos = y;
							if (newX >= diagX1 && newX < diagX2)
							{
								if (speedY < Math.max (lastSpeedY, accY))
									speedY = Math.max (lastSpeedY, accY) - diagRC * speedX;
							}
						
							FreeSpaceY = (i - (sizeY - 1)) - y - 1; 
							
							/*
							var s: String = newX + ", " + newY + ", " + i + ", " + FreeSpaceY + ', ' + diagX1 + ',' + diagX2;
							if (s != debugs)
							{
								trace (s);
								debugs = s;
							}
							*/
							
							if (FreeSpaceY > speedY)
							{
								//trace ("FreeSpaceY > speedY");
								FreeSpaceY = speedY;
							}
							else
								bounce ({top: true, left: false, right: false, bottom: false}, FreeSpaceX, FreeSpaceY, null);
					}
					/*
					else
						if (newX >= diagX1 && newX < diagX2)
						{
							//origYPos = y;
							FreeSpaceY = (i - (sizeY - 1)) - y - 1; 
							
							bounce ({top: true, left: false, right: false, bottom: false}, FreeSpaceX, FreeSpaceY, null);
						}
					*/
				
				/*
					if (curX < diagX1 + (W >> 1) && newY >= diagY1)
					{
						y = diagY1 - (sizeY - 1);
						bounce ({top: true, left: false, right: false, bottom: false}, FreeSpaceX, FreeSpaceY, null);
					}
					if (curX >= diagX2 - (W >> 1) && newY >= diagY2)
					{
						y = diagY2 - (sizeY - 1);
						bounce ({top: true, left: false, right: false, bottom: false}, FreeSpaceX, FreeSpaceY, null);
					}
				*/
				
					if (x < diagX1)
						diag = 0;
					if (x >= diagX2)
						diag = 0;
				}
				
			}
				
/*

              if (obj->iXPos < obj->iDiagLeftX)
                obj->iDiag = 0;
              if (obj->iXPos >= obj->iDiagRightX)
                obj->iDiag = 0;
            }	
			
			
*/
			
			
			
			
				
			

			
		}
	}


	public function run ()
	{
		var xp: Float;
		var yp: Float;
		var screenw: Float = Def.STAGE_W;
		var screenh: Float = Def.STAGE_H;

		if ((active) && (logicFrame == 0) && (layer != null))
		{
			if (animationTimeCounter == Def.BIGINT)
				animationTimeCounter = (objFlags.killAfterAnimation)? 0 : Def.globalTimeCounter;
			
			xp = (x + shiftX + ts.tileW / 2) * layer.scaleX;
			yp = (y + shiftY + ts.tileH / 2) * layer.scaleY;

			if (! objFlags.staticObject)
			{
				xp -= (layer.scrollX() + layer.shiftX - layer.timeCounter * layer.timeX);
				yp -= (layer.scrollY() + layer.shiftY - layer.timeCounter * layer.timeY);
			}

			if (layer.frameCounter > 0)
				if ((objFlags.onlyApearOffScreen) && 
						(xp > -layer.ts.tileW) && (xp < screenw + layer.ts.tileW) &&
						(yp > -layer.ts.tileH) && (yp < screenh + layer.ts.tileH))
					forget ();
				else
					onAppear ();
		}
		
		if ((active) && (waitTime == 0) && (moveDelay == 0))
		{
			if (! ignore)
			{
				origXPos = x;
				origYPos = y;

				if (skipFrameEvent > 0)
					skipFrameEvent--;
				else
					onFrame ();
				speedX += accX;
				speedY += accY;
				if (slowDownX > 0)
				{
					if (speedX > 0) speedX = Math.max (speedX - slowDownX, 0);
					if (speedX < 0) speedX = Math.min (speedX + slowDownX, 0);
				}
				if (slowDownY > 0)
				{
					if (speedY > 0) speedY = Math.max (speedY - slowDownY, 0);
					if (speedY < 0) speedY = Math.min (speedY + slowDownY, 0);
				}
				if (maxSpeedX != 0)
				{
					if (speedX > maxSpeedX)  speedX =  maxSpeedX;
					if (speedX < -maxSpeedX) speedX = -maxSpeedX;
				}
				if (maxSpeedY != 0)
				{
					if (speedY > maxSpeedY)  speedY =  maxSpeedY;
					if (speedY < -maxSpeedY) speedY = -maxSpeedY;
				}

				if (collisions != null)
				{
					checkBounceCollisions (collisions);
					if (collisions.length > 0)
						for (c in collisions)
						{
							c.obj1.runCollision (c.obj2, c.bounds);
							collisions.remove (c);
						}
				}
				
				checkLevelBounds ();
				
				x += speedX;
				y += speedY;

				checkDirection ();
			}

			if (animation - 1 >= 0)
			{
				if (! objFlags.frameBasedAnimation)
					animationTimeCounter++;
				if (animationTimeCounter >= ts.getAnimationFrameCount (animation - 1))
				{
					if (objFlags.killAfterAnimation)
						kill ();
					else
					{
						onAnimation ();
						if (animation - 1 >= 0)
							animationTimeCounter = animationTimeCounter % ts.getAnimationFrameCount (animation - 1);
					}
				}
			}

			
			timeCounter += 1;
			if (timeToLive > 0)
			{
				if (--timeToLive == 0)
					kill ();
			}
			if (blinkTime > 0)
				blinkTime--;

			if ((forgetX != 0) && (forgetY != 0) && (layer != null))
			{
				xp = x + shiftX + ts.tileW / 2;
				yp = y + shiftY + ts.tileH / 2;

				xp *= layer.scaleX;
				yp *= layer.scaleY;

				if (! objFlags.staticObject)
				{
					xp -= (layer.scrollX() + layer.shiftX - layer.timeCounter * layer.timeX);
					yp -= (layer.scrollY() + layer.shiftY - layer.timeCounter * layer.timeY);
				}

				var fx: Float = (layer.startObjX + forgetX) * layer.ts.tileW * layer.scaleX;
				var fy: Float = (layer.startObjY + forgetY) * layer.ts.tileH * layer.scaleY; 

				if ((xp < -fx) || (xp > screenw + fx) || 
					(yp < -fy) || (yp > screenh + fy))
				{
					//Def.log ("disappear: " + objName);
					onDisappear ();
					forget ();
				}
			}
		}
		
		logicFrame++;
		
		if (waitTime > 0)  waitTime--;
		if (moveDelay > 0)  moveDelay--;
	}

	
	public function draw ()
	{
		if ((active) && (waitTime == 0) && (animation > 0) && (logicFrame > 0))
		{
			if (! ((blinkTime > 0) && (Def.globalFrameCounter % 2 == 0)))
			{
				var flags = animFlags ^ (Def.TF_UPSIDEDOWN | Def.TF_MIRROR);
				if (Utils.xor (mirrorLeftRight,
								(objFlags.autoMirrorLeftRight) && lastDirectionLeft))
					flags = (flags ^ Def.TF_MIRROR) & Def.TF_FLAGS;
				if (Utils.xor (mirrorUpDown,
								(objFlags.autoMirrorUpDown) && lastDirectionUp))
					flags = (flags ^ Def.TF_UPSIDEDOWN) & Def.TF_FLAGS;

				var xp = x + shiftX + ts.tileW / 2 + offsetX;
				var yp = y + shiftY + ts.tileH / 2 + offsetY;

				if (((flags & Def.TF_MIRROR != 0) && objFlags.autoMirrorOffsetX))  
					xp += sizeX - 2 * offsetX - ts.tileW;

				if (((flags & Def.TF_UPSIDEDOWN != 0) && objFlags.autoMirrorOffsetY))
					yp += sizeY - 2 * offsetY - ts.tileH;

				if (layer != null)
				{
					xp *= layer.scaleX;
					yp *= layer.scaleY;
					if (! objFlags.staticObject)
					{
						xp -= (layer.scrollX() + layer.shiftX - layer.timeCounter * layer.timeX);
						yp -= (layer.scrollY() + layer.shiftY - layer.timeCounter * layer.timeY);
					}
				}

				var m = 3;
				// var HDir = -1;
				// var VDir = -1;
				if (flags & Def.TF_MIRROR != 0)
				{
					m -= 1;
					// HDir = 1;
				}
				if (flags & Def.TF_UPSIDEDOWN != 0)
				{
					m -= 2;
					// VDir = 1;
				}
				var tile = ts.getAnimationFrame (animation - 1, animationTimeCounter);
				// SetScale (layer.scaleX * HDir, layer.scaleY * VDir);

				if ((tile != lastTile) || (flags != lastFlags) || redrawBitmap)
				{
					if (tile == 0)
						surface.fillRect (new Rectangle (0, 0, ts.tileW, ts.tileH), 0);
					else
						if (ct == null)
							ts.drawTile (surface, 0, 0, tile - 1, m);
						else
						{
							surface.fillRect (new Rectangle (0, 0, ts.tileW, ts.tileH), 0);
							var bdTmp: BitmapData = new BitmapData (ts.tileW, ts.tileH, true, 0x0);
							ts.drawTile (bdTmp, 0, 0, tile - 1, m);
							surface.draw (bdTmp, null, ct);
							bdTmp = null;
						}
							
					
					
					
					#if flash9
						bitmap.visible = true;
					#elseif flash8
						bitmap._visible = true;
					#end

					//   surface.fillRect (new Rectangle (0, 0, ts.tileW, ts.tileH), 0x80FFFFFF);
					lastTile = tile;
					lastFlags = flags;
					redrawBitmap = false;
				}
				
				var sx: Float = 1.0;
				var sy: Float = 1.0;
				
				if (layer != null)
				{
					sx = layer.scaleX;
					sy = layer.scaleY;
				}
				#if flash9
					bitmap.alpha = alpha;
					bitmap.x = Std.int (xp - sx * ts.tileW / 2 + 0.95);  // FRACTION ???
					bitmap.y = Std.int (yp - sy * ts.tileH / 2 + 0.95);
					
					if (animation == Def.BIGINT)  // show bounding box
					{
						bitmap.bitmapData = new BitmapData (sizeX, sizeY, true, 0xFFFFFCC0);
						bitmap.x = x - ((layer.scrollX() + layer.shiftX - layer.timeCounter * layer.timeX));
						bitmap.y = y - ((layer.scrollY() + layer.shiftY - layer.timeCounter * layer.timeY));
						bitmap.scaleX = 1.0;
						bitmap.scaleY = 1.0;
					}
				#elseif flash8
					bitmap._alpha = 100 * alpha;
					bitmap._x = Std.int (xp - sx * ts.tileW / 2 + 0.95);  // FRACTION ???
					bitmap._y = Std.int (yp - sy * ts.tileH / 2 + 0.95);
				#end
			}
		}
		else
		{
			#if flash9
				bitmap.visible = false;
			#elseif flash8
				bitmap._visible = false;
			#end
		}
		
		if (framesToLive > 0)
		{
			if (--framesToLive == 0)
				kill ();
		}
		if (objFlags.frameBasedAnimation)
		{
			animationTimeCounter++;
			//trace (animationTimeCounter);
		}
	}


	public function checkBounceCollisions (Collisions: Array<Collision>)
	{
		if (active && ((bounceGroup /* | collideGroup */) != 0) && (! simple) && (!ignore))
		{
			var coll: Collision;
			var obj: Obj;
/* 
			var oldX1: Float = origXPos;
			var oldY1: Float = origYPos;
			var oldX2: Float = origXPos + sizeX - MARGIN;
			var oldY2: Float = origYPos + sizeY - MARGIN;
			var X1: Float = x;
			var Y1: Float = y;
			var X2: Float = x + sizeX - MARGIN;
			var Y2: Float = y + sizeY - MARGIN;
*/
			var oldX1: Float = x;
			var oldX2: Float = oldX1 + sizeX - FRACTION;
			var oldY1: Float = y;
			var oldY2: Float = oldY1 + sizeY - FRACTION;
			var X1: Float = oldX1 + speedX;
			var X2: Float = oldX2 + speedX;
			var Y1: Float = oldY1 + speedY;
			var Y2: Float = oldY2 + speedY;

			for (obj in objlist)
				if ((obj != this) && (obj.active) && (! obj.ignore))
				{
					if ((objFlags.staticObject == obj.objFlags.staticObject) &&
							((bounceGroup & obj.group) != 0)
					   ) 
					{
/*
						var oldx1: Float = obj.origXPos;
						var oldy1: Float = obj.origYPos;
						var oldx2: Float = obj.origXPos + obj.sizeX - MARGIN;
						var oldy2: Float = obj.origYPos + obj.sizeY - MARGIN;
						var x1: Float = obj.x;
						var y1: Float = obj.y;
						var x2: Float = obj.x + obj.sizeX - MARGIN;
						var y2: Float = obj.y + obj.sizeY - MARGIN;
*/

						var oldx1: Float = obj.x;
						var oldx2: Float = oldx1 + obj.sizeX - FRACTION;
						var oldy1: Float = obj.y;
						var oldy2: Float = oldy1 + obj.sizeY - FRACTION;
						var x1: Float = oldx1 + obj.speedX;
						var x2: Float = oldx2 + obj.speedX;
						var y1: Float = oldy1 + obj.speedY;
						var y2: Float = oldy2 + obj.speedY;

						if ((x1 <= X2) && (x2 >= X1) &&
							(y1 <= Y2) && (y2 >= Y1))
						{
							var Bnds1: Int = 0;
							var Bnds2: Int = 0;

							if (oldY1 > oldy2)
							{
								Bnds1 |= Def.UPPER_BOUND;
								Bnds2 |= Def.LOWER_BOUND;
							}
							if (oldX1 > oldx2) 
							{
								Bnds1 |= Def.LEFT_BOUND;
								Bnds2 |= Def.RIGHT_BOUND;
							}
							if (oldY2 < oldy1) 
							{
								Bnds1 |= Def.LOWER_BOUND;
								Bnds2 |= Def.UPPER_BOUND;
							}
							if (oldX2 < oldx1) 
							{
								Bnds1 |= Def.RIGHT_BOUND;
								Bnds2 |= Def.LEFT_BOUND;
							}

							if ((Bnds1 == (Def.LOWER_BOUND | Def.RIGHT_BOUND)) || (Bnds1 == (Def.UPPER_BOUND | Def.RIGHT_BOUND)))
							{
								if ((curHDir == 1) && (curVDir == 0))
								{
									Bnds1 &= ~Def.RIGHT_BOUND;
									Bnds2 &= ~Def.LEFT_BOUND;
								}
								if ((curHDir == 0) && (curVDir != 0)) 
								{
									Bnds1 &= ~Def.LOWER_BOUND;
									Bnds2 &= ~Def.UPPER_BOUND;
								}
							}
							if ((Bnds1 == (Def.LOWER_BOUND | Def.LEFT_BOUND)) || (Bnds1 == (Def.UPPER_BOUND | Def.LEFT_BOUND))) 
							{
								if ((curHDir == -1) && (curVDir == 0)) 
								{
									Bnds1 &= ~Def.LEFT_BOUND;
									Bnds2 &= ~Def.RIGHT_BOUND;
								}
								if ((curHDir == 0) && (curVDir != 0)) 
								{
									Bnds1 &= ~Def.LOWER_BOUND;
									Bnds2 &= ~Def.UPPER_BOUND;
								}
							}
							var l: Int = Collisions.length;
							//trace (Bnds2);
							Collisions[l] = { obj1: this, obj2: obj, bounds: Def.intToDirectionSet (Bnds2) };
							
							if (obj.simple)
								Collisions[l + 1] = { obj1: obj, obj2: this, bounds: Def.intToDirectionSet (Bnds1) }
						}
					}
			}
		}
		alreadyBounced = 0;
	}


	public function checkCollisionsOld (Collisions: Array<Collision>)
	{
		if (active && (( /* bounceGroup | */ collideGroup) != 0) && (! simple) && (!ignore))
		{
			var coll: Collision;
			var obj: Obj;

			var oldX1: Float = origXPos;
			var oldY1: Float = origYPos;
			var oldX2: Float = origXPos + sizeX - MARGIN;
			var oldY2: Float = origYPos + sizeY - MARGIN;
			var X1: Float = x;
			var Y1: Float = y;
			var X2: Float = x + sizeX - MARGIN;
			var Y2: Float = y + sizeY - MARGIN;

			for (obj in objlist)
				if ((obj != this) && (obj.active) && (! obj.ignore))
				{
					if ((objFlags.staticObject == obj.objFlags.staticObject) &&
						 (
							((bounceGroup & obj.group) != 0) 
							||
							 ((collideGroup & obj.group) != 0)
						 )
					   ) 
					{
						var oldx1: Float = obj.origXPos;
						var oldy1: Float = obj.origYPos;
						var oldx2: Float = obj.origXPos + obj.sizeX - MARGIN;
						var oldy2: Float = obj.origYPos + obj.sizeY - MARGIN;
						var x1: Float = obj.x;
						var y1: Float = obj.y;
						var x2: Float = obj.x + obj.sizeX - MARGIN;
						var y2: Float = obj.y + obj.sizeY - MARGIN;

						if ((x1 <= X2) && (x2 >= X1) &&
							(y1 <= Y2) && (y2 >= Y1))
						{
							var Bnds1: Int = 0;
							var Bnds2: Int = 0;

							if (oldY1 > oldy2)
							{
								Bnds1 |= Def.UPPER_BOUND;
								Bnds2 |= Def.LOWER_BOUND;
							}
							if (oldX1 > oldx2) 
							{
								Bnds1 |= Def.LEFT_BOUND;
								Bnds2 |= Def.RIGHT_BOUND;
							}
							if (oldY2 < oldy1) 
							{
								Bnds1 |= Def.LOWER_BOUND;
								Bnds2 |= Def.UPPER_BOUND;
							}
							if (oldX2 < oldx1) 
							{
								Bnds1 |= Def.RIGHT_BOUND;
								Bnds2 |= Def.LEFT_BOUND;
							}

							if ((Bnds1 == (Def.LOWER_BOUND | Def.RIGHT_BOUND)) || (Bnds1 == (Def.UPPER_BOUND | Def.RIGHT_BOUND)))
							{
								if ((curHDir == 1) && (curVDir == 0))
								{
									Bnds1 &= ~Def.RIGHT_BOUND;
									Bnds2 &= ~Def.LEFT_BOUND;
								}
								if ((curHDir == 0) && (curVDir != 0)) 
								{
									Bnds1 &= ~Def.LOWER_BOUND;
									Bnds2 &= ~Def.UPPER_BOUND;
								}
							}
							if ((Bnds1 == (Def.LOWER_BOUND | Def.LEFT_BOUND)) || (Bnds1 == (Def.UPPER_BOUND | Def.LEFT_BOUND))) 
							{
								if ((curHDir == -1) && (curVDir == 0)) 
								{
									Bnds1 &= ~Def.LEFT_BOUND;
									Bnds2 &= ~Def.RIGHT_BOUND;
								}
								if ((curHDir == 0) && (curVDir != 0)) 
								{
									Bnds1 &= ~Def.LOWER_BOUND;
									Bnds2 &= ~Def.UPPER_BOUND;
								}
							}
							var l: Int = Collisions.length;
							//trace (Bnds2);
							Collisions[l] = { obj1: this, obj2: obj, bounds: Def.intToDirectionSet (Bnds2) };
							
							if (obj.simple)
								Collisions[l + 1] = { obj1: obj, obj2: this, bounds: Def.intToDirectionSet (Bnds1) }
						}
					}
			}
		}
		alreadyBounced = 0;
	}
	
	
	public function setupOwnObjList ()
	{
		ownObjList = new List ();
		for (obj in objlist)
				if ((obj != this) && (obj.active) && (! obj.ignore))
					if ((objFlags.staticObject == obj.objFlags.staticObject) &&
						 (
						//	((bounceGroup & obj.group) != 0) 
						//	||
							 ((collideGroup & obj.group) != 0)
						 )
					   ) 
					{
						ownObjList.push (obj);
					}
	}


	public function checkCollisions (Collisions: Array<Collision>)
	{
		if (active && (( /* bounceGroup | */ collideGroup) != 0) && (! simple) && (!ignore))
		{
			var coll: Collision;
			var obj: Obj;

			var oldX1: Float = origXPos;
			var oldY1: Float = origYPos;
			var oldX2: Float = origXPos + sizeX - MARGIN;
			var oldY2: Float = origYPos + sizeY - MARGIN;
			var X1: Float = x;
			var Y1: Float = y;
			var X2: Float = x + sizeX - MARGIN;
			var Y2: Float = y + sizeY - MARGIN;
			
			var ol = objlist;
			if (ownObjList != null)
				ol = ownObjList;

			for (obj in ol)
				if ((obj != this) && (obj.active) && (! obj.ignore))
				{
					if ((objFlags.staticObject == obj.objFlags.staticObject) &&
						 (
						//	((bounceGroup & obj.group) != 0) 
						//	||
							 ((collideGroup & obj.group) != 0)
						 )
					   ) 
					{
						var oldx1: Float = obj.origXPos;
						var oldy1: Float = obj.origYPos;
						var oldx2: Float = obj.origXPos + obj.sizeX - MARGIN;
						var oldy2: Float = obj.origYPos + obj.sizeY - MARGIN;
						var x1: Float = obj.x;
						var y1: Float = obj.y;
						var x2: Float = obj.x + obj.sizeX - MARGIN;
						var y2: Float = obj.y + obj.sizeY - MARGIN;

						if ((x1 <= X2) && (x2 >= X1) &&
							(y1 <= Y2) && (y2 >= Y1))
						{
							var Bnds1: Int = 0;
							var Bnds2: Int = 0;

							if (oldY1 > oldy2)
							{
								Bnds1 |= Def.UPPER_BOUND;
								Bnds2 |= Def.LOWER_BOUND;
							}
							if (oldX1 > oldx2) 
							{
								Bnds1 |= Def.LEFT_BOUND;
								Bnds2 |= Def.RIGHT_BOUND;
							}
							if (oldY2 < oldy1) 
							{
								Bnds1 |= Def.LOWER_BOUND;
								Bnds2 |= Def.UPPER_BOUND;
							}
							if (oldX2 < oldx1) 
							{
								Bnds1 |= Def.RIGHT_BOUND;
								Bnds2 |= Def.LEFT_BOUND;
							}

							if ((Bnds1 == (Def.LOWER_BOUND | Def.RIGHT_BOUND)) || (Bnds1 == (Def.UPPER_BOUND | Def.RIGHT_BOUND)))
							{
								if ((curHDir == 1) && (curVDir == 0))
								{
									Bnds1 &= ~Def.RIGHT_BOUND;
									Bnds2 &= ~Def.LEFT_BOUND;
								}
								if ((curHDir == 0) && (curVDir != 0)) 
								{
									Bnds1 &= ~Def.LOWER_BOUND;
									Bnds2 &= ~Def.UPPER_BOUND;
								}
							}
							if ((Bnds1 == (Def.LOWER_BOUND | Def.LEFT_BOUND)) || (Bnds1 == (Def.UPPER_BOUND | Def.LEFT_BOUND))) 
							{
								if ((curHDir == -1) && (curVDir == 0)) 
								{
									Bnds1 &= ~Def.LEFT_BOUND;
									Bnds2 &= ~Def.RIGHT_BOUND;
								}
								if ((curHDir == 0) && (curVDir != 0)) 
								{
									Bnds1 &= ~Def.LOWER_BOUND;
									Bnds2 &= ~Def.UPPER_BOUND;
								}
							}
							var l: Int = Collisions.length;
							//trace (Bnds2);
							Collisions[l] = { obj1: this, obj2: obj, bounds: Def.intToDirectionSet (Bnds2) };
							
							if (obj.simple)
								Collisions[l + 1] = { obj1: obj, obj2: this, bounds: Def.intToDirectionSet (Bnds1) }
						}
					}
			}
		}
		alreadyBounced = 0;
	}


	public function runCollision (obj: Obj, ds: DirectionSet)
	{
		if ((bounceGroup & obj.group) != 0)
			bounce (ds, 0, 0, obj);

		if ((collideGroup & obj.group) != 0)
			onCollide (obj, ds);
	}


	public function kill ()
	{
		onKill ();
		if ((tileToPutBack >= 0) && (layer != null))
		{
			layer.mapData[mapY][mapX] = tileToPutBack;
			layer.tilesToRedraw.add (new Point (mapX, mapY));
		}
		deleteObj ();
	}


	public function forget ()
	{
		//Def.log ("forget " + objName + " " + objCode);
		if ((layer != null) && (mapX >= 0) && (mapY >= 0))
		{
			if ((objCode > 0) && (objCode <= 255))
			{
				layer.writeBoundMap (mapX, mapY, objCode);

				// layer.FBoundMapData[mapY][mapX] = layer.FBoundMapData[mapY][mapX] | (objCode << 8);

				if ((objPar > 0) && (objPar <= 255))
					layer.writeBoundMap (mapX, mapY - 1, objPar);

				//  layer.FBoundMapData[mapY - 1][mapX] = layer.FBoundMapData[mapY - 1][mapX] | (objPar << 8);
			}
		}
		deleteObj ();  // ???
	}
  
  
	public function deleteObj ()
	{
		onDestroy ();
		active = false;
		for (msg in msglist)
		{
			switch (msg.target)
			{
				case mtObj (o): if (this == o) msglist.remove (msg);
				default: 
			}
		}
		if (focus != null)
			if (this == focus.objInFocus)
				clearObjectFocus ();
		objlist.remove (this);
		for (obj in objlist)
			if (obj.ownObjList != null)
				obj.ownObjList.remove (this);
		#if flash9
			bitmap.visible = false;
			if (Bitmap != null) mcContainer.removeChild (bitmap);
		#elseif flash8
			bitmap._visible = false;
			bitmap.unloadMovie ();
		#end
		bitmap = null;
		mcContainer = null;
		surface = null;
		collideTiles = null;
		collisions = null;
	}


	public static function iterator ()
	{
		return objlist.iterator ();
	}

}

