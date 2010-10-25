package gamelib2d;

import gamelib2d.Def;
import flash.display.BitmapData;
import flash.geom.Point;
import flash.geom.Rectangle;
import flash.geom.ColorTransform;

#if flash8
	import flash.MovieClip;
#elseif flash9
	import flash.display.Sprite;
	import flash.display.Bitmap;
	import flash.display.BlendMode;
#end
 

enum MapRepeatPattern
{
	mrp_none;
	mrp_tile;
	mrp_extend;
	mrp_mirror;
	mrp_zigzag;
	mrp_tilemir;
	mrp_rotate;
	mrp_invmir;
}

enum BlockPattern
{
	bp_none;
	bp_mirrormap;
	bp_mirrortiles;
	bp_mirrormaptiles;
}


class Layer
{
	public static var layers: List<Layer> = new List ();
	
	public var ts: TileSet;
	public var surface: BitmapData;

	#if flash9
		private var mcContainer: Sprite;
		public var lyr: Sprite;
		private var mc00: Sprite;
		private var mc01: Sprite;
		private var mc10: Sprite;
		private var mc11: Sprite;
	#elseif flash8
		private var mcContainer: MovieClip;
		public var lyr: MovieClip;
		private var mc00: MovieClip;
		private var mc01: MovieClip;
		private var mc10: MovieClip;
		private var mc11: MovieClip;
	#end

	public var name: String;
	
	public var active: Bool;

	public var mapW: Int;
	public var mapH: Int;
	public var shiftX: Int;
	public var shiftY: Int;
	public var frameCounter: Int;
	public var timeCounter: Int;
	public var timeX: Float;
	public var timeY: Float;
	public var scaleX: Float;
	public var scaleY: Float;
	public var tileW: Float;
	public var tileH: Float;

	public var canScrollHorz: Bool;
	public var canScrollVert: Bool;

	public var hrp: MapRepeatPattern;
	public var vrp: MapRepeatPattern;

	private var fXPos: Float;
	private var fYPos: Float;
	private var fNewX: Float;
	private var fNewY: Float;
	
	private var bufW: Int;
	private var bufH: Int;

	private var mapScreenTiles: Array<Array<Int>>;

	public var mapData: Array<Array<Int>>;
	public var boundMapData: Array<Array<Int>>;

	public var blockW: Int;
	public var blockH: Int;
	public var hbp: BlockPattern;
	public var vbp: BlockPattern;
	public var metaMapW: Int;
	public var metaMapH: Int;
	public var metaMap: Array<Array<Array<Int>>>;
	public var metaCodes: Array<Array<Int>>;
	public var metaGapW: Int;
	public var metaGapH: Int;
	public var metaSkipW: Int;
	public var metaSkipH: Int;

	public var curMapX: Int;
	public var curMapY: Int;
	public var curFlags: Int;
	public var curTile: Int;
	public var curSeq: Int;
	public var curBounds: Int;
	public var curMetaX: Int;
	public var curMetaY: Int;

	#if flash9
		public var tilesToRedraw: List<Point>;
		public var mapCodeList: List<Point>;
	#elseif flash8
		public var tilesToRedraw: List<Point<Int>>;
		public var mapCodeList: List<Point<Int>>;
	#end

	private var lastMapX1: Int;
	private var lastMapX2: Int;
	private var lastMapY1: Int;
	private var lastMapY2: Int;

	public var startObjX: Int;
	public var startObjY: Int;
	public var startObjCode: Int;

	public var resultX: Int;
	public var resultY: Int;


	public function new (mc)
	{
		mcContainer = mc;
		layers.add (this);
		active = false;
	}

	public function clear ()
	{
		layers.remove (this);
		if (ts != null)  { ts.clear ();  ts = null; } 
		mapScreenTiles = null;
		mapData = null;
		boundMapData = null;
		
		if (mc00 != null)  { mcContainer.removeChild (mc00);  mc00 = null; }
		if (mc01 != null)  { mcContainer.removeChild (mc01);  mc01 = null; }
		if (mc10 != null)  { mcContainer.removeChild (mc10);  mc10 = null; }
		if (mc11 != null)  { mcContainer.removeChild (mc11);  mc11 = null; }
		
		lyr = null;
		tilesToRedraw = null;
		mapCodeList = null;
		active = false;
	}

	public function init (tileset: TileSet, data: MapData, ?transparent: Bool = true, 
							?xscale: Float, ?yscale: Float, ?horzrp: MapRepeatPattern, ?vertrp: MapRepeatPattern,
							?hscroll: Bool, ?vscroll: Bool)
	{
		var i: Int;
		var j: Int;

		ts = tileset;

		//name = ts.name;

		mapW = data.mapW;
		mapH = data.mapH;

		name = data.name;
		mapData = data.mapdata ();
		boundMapData = data.boundmapdata ();

		setMetaMap (0, 0, [], [], bp_none, bp_none);

		if (xscale != null)  scaleX = xscale;  else  scaleX = 1.0;
		if (yscale != null)  scaleY = yscale;  else  scaleY = 1.0;

		timeX = 0;
		timeY = 0;

		tileW = ts.tileW * scaleX;
		tileH = ts.tileH * scaleY;

		#if flash8
			bufW = Std.int ((Def.STAGE_W + tileW - 1) / tileW) + 2;
			bufH = Std.int ((Def.STAGE_H + tileH - 1) / tileH) + 2;
		#elseif flash9
			bufW = Std.int ((Def.STAGE_W + tileW - 1) / tileW) + 2;
			bufH = Std.int ((Def.STAGE_H + tileH - 1) / tileH) + 2;
		#end
		//if (bufW > mapW) bufW = mapW;
		//if (bufH > mapH) bufH = mapH;

		if (hscroll != null)  canScrollHorz = hscroll;  else  canScrollHorz = true;
		if (vscroll != null)  canScrollVert = vscroll;  else  canScrollVert = true;

		if (!canScrollHorz)  bufW--;
		if (!canScrollVert)  bufH--;

		mapScreenTiles = new Array ();
		for (j in 0...bufH)
		{
			mapScreenTiles[j] = new Array ();
			for (i in 0...bufW)
				mapScreenTiles[j][i] = 0;
		}

		surface = new BitmapData (bufW * ts.tileW, bufH * ts.tileH, transparent, 0x0);

		#if flash9
			mc00 = new Sprite ();
			mc00.addChild (new Bitmap (surface));
			mcContainer.addChild (mc00);
		#elseif flash8
			var Depth = flash.Lib._root.getNextHighestDepth ();
			mc00 = mcContainer.createEmptyMovieClip (name + "_00", Depth+1);
			mc00.attachBitmap (surface, Depth+1);
		#end

		if (hscroll)
		{
			#if flash9
				mc01 = new Sprite ();
				mc01.addChild (new Bitmap (surface));
				mcContainer.addChild (mc01);
			#elseif flash8
				mc01 = mcContainer.createEmptyMovieClip (name + "_01", Depth+2);
				mc01.attachBitmap (surface, Depth+2);
			#end
		}

		if (vscroll)
		{
			#if flash9
				mc10 = new Sprite ();
				mc10.addChild (new Bitmap (surface));
				mcContainer.addChild (mc10);
			#elseif flash8
				mc10 = mcContainer.createEmptyMovieClip (name + "_10", Depth+3);
				mc10.attachBitmap (surface, Depth+3);
			#end

			if (hscroll)
			{
				#if flash9
					mc11 = new Sprite ();
					mc11.addChild (new Bitmap (surface));
					mcContainer.addChild (mc11);
				#elseif flash8
					mc11 = mcContainer.createEmptyMovieClip (name + "_11", Depth+4);
					mc11.attachBitmap (surface, Depth+4);
				#end
			}
		}

		#if flash9
			mc00.scaleX = scaleX;
			mc00.scaleY = scaleY;
			mc00.cacheAsBitmap = true;
			if (hscroll)
			{
				mc01.scaleX = scaleX;
				mc01.scaleY = scaleY;
				mc01.cacheAsBitmap = true;
			}
			if (vscroll)
			{
				mc10.scaleX = scaleX;
				mc10.scaleY = scaleY;
				mc10.cacheAsBitmap = true;
				if (hscroll)
				{
					mc11.scaleX = scaleX;
					mc11.scaleY = scaleY;
					mc11.cacheAsBitmap = true;
				}
			}

			lyr = new Sprite ();
			mcContainer.addChild (lyr);
		#elseif flash8
			mc00._xscale = 100 * scaleX;
			mc00._yscale = 100 * scaleY;
			mc00.cacheAsBitmap = true;
			if (hscroll)
			{
				mc01._xscale = 100 * scaleX;
				mc01._yscale = 100 * scaleY;
				mc01.cacheAsBitmap = true;
			}
			if (vscroll)
			{
				mc10._xscale = 100 * scaleX;
				mc10._yscale = 100 * scaleY;
				mc10.cacheAsBitmap = true;
				if (hscroll)
				{
					mc11._xscale = 100 * scaleX;
					mc11._yscale = 100 * scaleY;
					mc11.cacheAsBitmap = true;
				}
			}
			lyr = mcContainer;  //.createEmptyMovieClip (name + "_layer", Depth+5);
		#end

		fXPos = 0;
		fYPos = 0;

		timeCounter = 0;
		frameCounter = 0;

		shiftX = 0;
		shiftY = 0;

		fNewX = 0;
		fNewY = 0;

		if (horzrp != null) hrp = horzrp; else hrp = mrp_none;
		if (vertrp != null) vrp = vertrp; else vrp = mrp_none;

		tilesToRedraw = new List ();
		mapCodeList = new List ();

		lastMapX1 = 0;
		lastMapX2 = 0;
		lastMapY1 = 0;
		lastMapY2 = 0;

		startObjX = 3;
		startObjY = 3;
		startObjCode = 0x10;
		
		active = true;
	}


	// code, par, mapx, mapy
	public var InitObjectFunction: Int -> Int -> Int -> Int -> Void;

	public function scanForNewObjects (?scanAll: Bool = false)
	{
		var i, j: Int;
		var lasti: Int = 0;
		var lastj: Int = 0;
		var code, lastcode: Int;
		var x, y: Int;
		var oldx, oldy: Int;
		var x1, y1, x2, y2: Int;
		var alwx, alwy: Bool;
		var cx, cy, cya: Int;
		var pass: Int = 0;
		
		var ScanAll: Bool = scanAll;
		if (lastMapX1 == lastMapX2 || lastMapY1 == lastMapY2) 
		{
			ScanAll = true;
			mapCodeList.clear ();
		}

		oldx = Std.int (fXPos / tileW) - 1;
		oldy = Std.int (fYPos / tileH) - 1;
		x = Std.int ((fNewX + shiftX - timeCounter * timeX) / tileW) - 1;
		y = Std.int ((fNewY + shiftY - timeCounter * timeY) / tileH) - 1;

		x1 = x + 1 - startObjX;
		x2 = x + bufW - 1 + startObjX;
		y1 = y + 1 - startObjY;
		y2 = y + bufH - 1 + startObjY;

		for (i in x1 ... x2 + 1)
		{
			alwx = ScanAll;
			if (x < oldx)
				if (i < lastMapX1)
					alwx = true;
			if (x > oldx)
				if (i > lastMapX2)
					alwx = true;

			lastcode = -1;
			//alwx = true;
			//alwy = true;

			for (j in y1 ... y2 + 1)
			{
				alwy = ScanAll;
				if (y < oldy)
					if (j < lastMapY1)
						alwy = true;
				if (y > oldy)
					if (j > lastMapY2)
						alwy = true;

				if (! mapCodeList.isEmpty())
					for (p in mapCodeList)
						if ((p.x == x) && (p.y == y))
						{
							alwy = true;
							mapCodeList.remove (p);
						}

				if (alwx || alwy)
				{
					pass++;
					cy = 0; // undefined

					code = readBoundMap (i, j) >> 8;
					if (code >= startObjCode)
					{
						cx = curMapX;
						cy = curMapY;
						cya = cy - 1;

						if ((lastcode == -1) || (lasti != i) || (lastj != j - 1))
						{
							lastcode = readBoundMap (i, j - 1) >> 8;
							cya = curMapY;
						}
						if (lastcode >= startObjCode)
							lastcode = 0;

						InitObjectFunction (code, lastcode, cx, cy);

						boundMapData[cy][cx] = boundMapData[cy][cx] & 0x00FF;
						if (lastcode > 0)
							boundMapData[cya][cx] = boundMapData[cya][cx] & 0x00FF;

						code = 0;
					}
					lasti = i;
					lastj = j;
					lastcode = code;
					cya = cy;
				}	
			}

		}
		mapCodeList.clear ();

		//if (pass != 0) trace(pass);

		lastMapX1 = x1;
		lastMapY1 = y1;
		lastMapX2 = x2;
		lastMapY2 = y2;
	}



	public function width ()
	{
		return metaMapW * blockW * tileW;
	}


	public function height ()
	{
		return metaMapH * blockH * tileH;
	}


	public function moveTo (X: Float, Y: Float)
	{
		fNewX = Std.int (X);
		fNewY = Std.int (Y);
	}


	public function scrollX ()
	{
		return fNewX;
	}


	public function scrollY ()
	{
		return fNewY;
	}


	public function update ()
	{
		timeCounter++;
	}


	public function setOpaqueBackground (color: Int)
	{
	#if flash9
		mc00.opaqueBackground = color;
		if (mc01 != null) mc01.opaqueBackground = color;
		if (mc10 != null) mc10.opaqueBackground = color;
		if (mc11 != null) mc11.opaqueBackground = color;
	#elseif flash8
		mc00._opaqueBackground = color;
		if (mc01 != null) mc01._opaqueBackground = color;
		if (mc10 != null) mc10._opaqueBackground = color;
		if (mc11 != null) mc11._opaqueBackground = color;
	#end
	}


	public function setAlpha (a: Float)
	{
	#if flash9
		mc00.alpha = a;
		if (mc01 != null) mc01.alpha = a;
		if (mc10 != null) mc10.alpha = a;
		if (mc11 != null) mc11.alpha = a;
	#elseif flash8
		mc00._alpha = a;
		if (mc01 != null) mc01._alpha = a;
		if (mc10 != null) mc10._alpha = a;
		if (mc11 != null) mc11._alpha = a;
	#end
	}


	public function setBlendMode (bm)  // bm: BlendMode
	{
	#if flash9
		mc00.blendMode = bm;
		if (mc01 != null) mc01.blendMode = bm;
		if (mc10 != null) mc10.blendMode = bm;
		if (mc11 != null) mc11.blendMode = bm;
	#elseif flash8
		mc00.blendMode = bm;
		if (mc01 != null) mc01.blendMode = bm;
		if (mc10 != null) mc10.blendMode = bm;
		if (mc11 != null) mc11.blendMode = bm;
	#end
	}

	
	public function setColorTransform (ct: ColorTransform)
	{
		mc00.transform.colorTransform = ct;
		if (mc01 != null) mc01.transform.colorTransform = ct;
		if (mc10 != null) mc10.transform.colorTransform = ct;
		if (mc11 != null) mc11.transform.colorTransform = ct;
	}
	
	public function getColorTransform (): ColorTransform
	{
		return mc00.transform.colorTransform;
	}


	public function setMetaMap (w: Int, h: Int, map: Array<Array<Array<Int>>>, codes: Array<Array<Int>>, horzbp: BlockPattern, vertbp: BlockPattern, ?gapW: Int = 0, ?gapH: Int = 0, ?skipW: Int = 0, ?skipH: Int = 0)
	{
		blockW = w;
		if (blockW <= 0) blockW = mapW;
		blockH = h;
		if (blockH <= 0) blockH = mapH;
		metaMap = map;
		metaCodes = codes;
		curMetaX = 0;
		curMetaY = 0;
		if (metaMap.length == 0)
		{
			metaMap = [[[1, 1]]];
			metaCodes = [[0]];
		}
		metaMapH = metaMap.length;
		metaMapW = metaMap[0].length;
		hbp = horzbp;
		vbp = vertbp;
		metaGapW = gapW;
		metaGapH = gapH;
		metaSkipW = skipW;
		metaSkipH = skipH;
	}
	

	public function expandMetaMap (?compress: Int = 0)
	{
		var newMap: Array<Array<Int>> = new Array ();
		var newBoundMap: Array<Array<Int>> = new Array ();
		
		for (j in 0 ... metaMapH * blockH)
		{
			newMap[j] = new Array ();
			newBoundMap[j] = new Array ();
			for (i in 0 ... metaMapW * blockW)
			{
				curMapX = i;
				curMapY = j;
				curTile = 0;
				curFlags = 0;
				
				if (findCurPosInMap ())
				{
					newMap[j][i] = mapData[curMapY][curMapX] ^ curFlags;
					var v: Int = boundMapData[curMapY][curMapX];
					if (curFlags & Def.TF_MIRROR != 0)  // turn around bounds in mirrored tile
					{
						var l: Bool = v & Def.LEFT_BOUND != 0;
						var r: Bool = v & Def.RIGHT_BOUND != 0;
						v &= ~(Def.LEFT_BOUND | Def.RIGHT_BOUND);
						if (l) v |= Def.RIGHT_BOUND;
						if (r) v |= Def.LEFT_BOUND;
					}
					if (curFlags & Def.TF_UPSIDEDOWN != 0) 
					{
						var u: Bool = v & Def.UPPER_BOUND != 0;
						var l: Bool = v & Def.LOWER_BOUND != 0;
						v &= ~(Def.UPPER_BOUND | Def.LOWER_BOUND);
						if (u) v |= Def.LOWER_BOUND;
						if (l) v |= Def.UPPER_BOUND;
					}
					if ((v >> 8) & 0xFF == 0xFF)
						v = (metaCodes[curMetaY][Std.int (curMetaX / 3)] << 8) | (v & 0xFF);
					newBoundMap[j][i] = v;
				}
				else
				{
					newMap[j][i] = 0;
					newBoundMap[j][i] = 0;
				}
			}
		}
		
		//Def.log("" + metaMapW/3 * blockW);
		if (compress != 0)
		{
			metaMapW = Std.int (metaMapW / compress);
			//Def.log("" + metaMapW);
			
			for (l in 0...metaMapH * blockH)
			{
				var aiMapLine: Array<Int> = new Array ();
				var aiBoundMapLine: Array<Int> = new Array ();
				
				for (i in 0...metaMapW)
				{
					for (j in 0...blockW)
					{
						var m: Int = i * blockW + j;
						aiMapLine[m] = 0;
						aiBoundMapLine[m] = 0;
						for (k in 0...compress)
						{
							var x: Int = i * compress * blockW + k * blockW + j;
							if ((newMap[l][x] & ~Def.TF_FLAGS) != 0)
								aiMapLine[m] = newMap[l][x];
							if (newBoundMap[l][x] != 0)
							{
								aiBoundMapLine[m] |= (newBoundMap[l][x] & 0xFF);
								if ((newBoundMap[l][x] >> 8) != 0)
									aiBoundMapLine[m] = (newBoundMap[l][x] & 0xFFFFFF00) | (aiBoundMapLine[m] & 0xFF);
							}
						}
					}
				}
				newMap[l] = aiMapLine;
				newBoundMap[l] = aiBoundMapLine;
			}
		}
		
		mapW = metaMapW * blockW;
		mapH = metaMapH * blockH;
		
		mapData = newMap;
		boundMapData = newBoundMap;
		
		setMetaMap (0, 0, [], [], bp_none, bp_none);
	}

	
	public function findCurPosInMap ()   // curMapX, curMapY, curTile, curFlags
	{
		var x: Int;
		var y: Int;
		var i: Int;
		var mMapW: Int = metaMapW * blockW;
		var mMapH: Int = metaMapH * blockH;

		x = curMapX;
		switch (hrp)
		{
			case mrp_none:	 	if ((x < 0) || (x > mMapW - 1)) return false;
			case mrp_tile:		x = Utils.safeMod (x, mMapW);
			case mrp_extend:	if (x < 0) x = 0; else if (x > mMapW - 1) x = mMapW - 1;
			case mrp_tilemir:	i = Utils.safeMod (x + mMapW, (mMapW * 4));
								x = Utils.iAbs (i - 2 * mMapW);
								if (i < 2 * mMapW)
								{
									curFlags = curFlags | Def.TF_MIRROR;
									x--;
								}
			case mrp_invmir:	x = mMapW - 1 - x;
								curFlags = curFlags ^ Def.TF_MIRROR;
			default:			x = curMapX;
		}
		i = Utils.safeMod (x + mMapW, mMapW * 2);
		if (vrp == mrp_rotate)
			if (Utils.safeMod (curMapY + mMapH, 2 * mMapH) < mMapH) i = Utils.safeMod (i + mMapW, 2 * mMapW);
		if (vrp != mrp_zigzag)
			if (i < mMapW) curFlags = curFlags ^ Def.TF_MIRROR;
		x = Utils.iAbs (i - mMapW);
		if (i < mMapW)
			x--;

		y = curMapY;
		switch (vrp)
		{
			case mrp_none:		if ((y < 0) || (y > mMapH - 1)) return false;
			case mrp_tile:		y = Utils.safeMod (y, mMapH);
			case mrp_extend:	if (y < 0) y = 0; else if (y > mMapH - 1) y = mMapH - 1;
			case mrp_tilemir:	i = Utils.safeMod (y + mMapH, (mMapH * 4));
								y = Utils.iAbs (i - 2 * mMapH);
								if (i < 2 * mMapH)
								{
									curFlags = curFlags | Def.TF_UPSIDEDOWN;
									y--;
								}
			case mrp_invmir:	y = mMapH - 1 - y;
								curFlags = curFlags ^ Def.TF_UPSIDEDOWN;
			default:			y = curMapY;
		}
		i = Utils.safeMod (y + mMapH, mMapH * 2);
		if (hrp == mrp_rotate)
			if (Utils.safeMod (curMapX + mMapW, 2 * mMapW) < mMapW) i = Utils.safeMod (i + mMapH, 2 * mMapH);
		if (hrp != mrp_zigzag)
			if (i < mMapH) curFlags = curFlags ^ Def.TF_UPSIDEDOWN;
		y = Utils.iAbs (i - mMapH);
		if (i < mMapH)
			y--;

		var srcBlockW: Int = blockW + metaGapW;
		var srcBlockH: Int = blockH + metaGapH;
		
		curMetaX = Std.int (x / blockW);
		curMetaY = Std.int (y / blockH);
		
		i = metaMap[curMetaY][curMetaX][0];
		if (i == 0) return false;
		if (i < 0)
		{
			if ((hbp == bp_mirrormap) || (hbp == bp_mirrormaptiles)) x = srcBlockW - 1 - x;
			if ((hbp == bp_mirrortiles) || (hbp == bp_mirrormaptiles)) curFlags = curFlags ^ Def.TF_MIRROR;
		}
		x = Utils.safeMod (x, blockW) + (Utils.iAbs (i) - 1) * srcBlockW + metaSkipW;

		i = metaMap[curMetaY][curMetaX][1];
		if (i == 0) return false;
		if (i < 0)
		{
			if ((vbp == bp_mirrormap) || (vbp == bp_mirrormaptiles)) y = srcBlockH - 1 - y;
			if ((vbp == bp_mirrortiles) || (vbp == bp_mirrormaptiles)) curFlags = curFlags ^ Def.TF_UPSIDEDOWN;
		}
		y = Utils.safeMod (y, blockH) + (Utils.iAbs (i) - 1) * srcBlockH + metaSkipH;

		curMapX = Utils.safeMod (x, mapW);
		curMapY = Utils.safeMod (y, mapH);
		return true;
	}


	public function getCurTile (x: Int, y: Int)
	{
		curMapX = x;
		curMapY = y;
		curTile = 0;
		curFlags = 0;

		if (findCurPosInMap ())
		{
			curTile = mapData[curMapY][curMapX];
			curSeq = -1;
			if (curTile < 0)
			{
				curSeq = -curTile - 1;
				curTile = ts.GetSequenceFrame (curSeq);
				curFlags = curFlags | Def.TF_SEQUENCE;
			}
			else
				if (curTile == 0)
					curFlags = 0;
					
			if (curTile & Def.TF_FLAGS != 0)
			{
				curFlags ^= (curTile & Def.TF_FLAGS);
				curTile &= (~Def.TF_FLAGS);
			}
			
		}
		return curTile | curFlags;
	}


	public function writeMap (x: Int, y: Int, tile: Int)
	{
		//trace (mapData[y][x] + ", " + tile);
		mapData[y][x] = tile;
		tilesToRedraw.add (new Point (x, y));
	}


	public function writeBoundMap (x: Int, y: Int, code: Int, ?bounds: Int = 0xFFFFFFFF)
	{
		if (code != -1)
			boundMapData[y][x] = (boundMapData[y][x] & 0x000000FF) | (code << 8);
		if (bounds != 0xFFFFFFFF)
			boundMapData[y][x] = (boundMapData[y][x] & 0xFFFFFF00) + bounds;

		//var Code: Int = ReadBoundMap (X, Y);
		//boundMapData[curMapY][curMapX] = boundMapData[curMapY][curMapX] | (code << 8);

		if (code >= startObjCode)
			mapCodeList.add (new Point (x, y));
	}


	public function readBoundMap (x: Int, y: Int, ?edgeValue: Int, ?bounds: Int, ?code: Int): Int
	{
		var mMapW: Int = metaMapW * blockW;
		var mMapH: Int = metaMapH * blockH;
		
		if (edgeValue == null) edgeValue = 0xFFFF;
		if (edgeValue == -1)
		{
			if (hrp == mrp_none) x = Utils.safeMod (x, mMapW);
			if (vrp == mrp_none) y = Utils.safeMod (y, mMapH);
		}

		curMapX = x;
		curMapY = y;
		if (!findCurPosInMap ())
		{
			curBounds = 0;
			if (y < 0)          if (edgeValue & Def.LOWER_BOUND != 0)  curBounds |= Def.LOWER_BOUND;
			if (y > mMapH - 1)  if (edgeValue & Def.UPPER_BOUND != 0)  curBounds |= Def.UPPER_BOUND;
			if (x < 0)          if (edgeValue & Def.RIGHT_BOUND != 0)  curBounds |= Def.RIGHT_BOUND;
			if (x > mMapW - 1)  if (edgeValue & Def.LEFT_BOUND  != 0)  curBounds |= Def.LEFT_BOUND;
			edgeValue &= curBounds;
			return edgeValue;
		}
		else
		{
			if (bounds == null) bounds = -1;
			if (code == null) code = -1;

			curBounds = boundMapData[curMapY][curMapX];
			curTile = mapData[curMapY][curMapX];
			curSeq = -1;
			if (curTile < 0)
			{
				curSeq = -curTile - 1;
				///curBounds = ts.GetSequenceFrameBounds (curSeq); 
				/// changed Nov 2009:
				curBounds = 0;
			}
			if (bounds != -1)
			{
				if (bounds & Def.GRP_DIAG_BOUNDS != 0)
					if (curBounds & 0x00F0 != 0)
					{
						curBounds |= 0x0000000F;
						curBounds &= 0xFFFFFF0F;
					}
				if (bounds & Def.GRP_STATIC_BOUNDS == 0) 
					curBounds &= 0xFF00;
				if (bounds & Def.GRP_MAP_CODES == 0)
					curBounds &= 0x00FF; 
				else
				{
					if ((curBounds >> 8) > 0)
					{
						curBounds |= code;
						return curBounds;
					}
				}
			}
			if (curBounds & 0x00F0 != 0) curBounds = (curBounds << 16) | 0x00F0;  // diagonal bounds
				return curBounds;
		}
	}
	
	
	public function countMapCode (code: Int): Int
	{
		var mMapW: Int = metaMapW * blockW;
		var mMapH: Int = metaMapH * blockH;
		var counter: Int = 0;
		
		for (j in 0...mMapH)
			for (i in 0...mMapW)
				if ((readBoundMap (i, j) >> 8) == code)
					counter++;
		return counter;
	}


	public function findMapCode (code: Int, ?above: Int = -1): Bool
	{
		var mMapW: Int = metaMapW * blockW;
		var mMapH: Int = metaMapH * blockH;
		
		for (j in 0...mMapH)
			for (i in 0...mMapW)
				if ((readBoundMap (i, j) >> 8) == code)
					if (above == -1 || (readBoundMap (i, j - 1) >> 8) == above)
					{
						resultX = i;
						resultY = j;
						return true;
					}
		return false;
	}


	public function draw (?force: Bool = false)
	{
		var i: Int;
		var j: Int;
		var x: Int;
		var y: Int;
		var bufx: Int;
		var bufy: Int;
		var count: Int;
		var limit: Int;
		var oldtile: Int;
		var alwx, alwy: Bool;
		var oldx, oldy: Float;
		var m: Int;
		var pass: Int;
		
		if (frameCounter == 0)
			limit = bufH * bufW;
		else
			limit = bufH + bufW;

		oldx = fXPos;
		oldy = fYPos;

		fXPos = fNewX + shiftX - timeCounter * timeX;
		fYPos = fNewY + shiftY - timeCounter * timeY;

		count = 0;
		pass = 0;

		var xblocks: Int = Std.int (fXPos / tileW) - Std.int (oldx / tileW);
		var yblocks: Int = Std.int (fYPos / tileH) - Std.int (oldy / tileH);

		if (name != "EmptyMap")
			for (j in 0...bufH)
			{
				alwy = false;
				if (canScrollVert)
				{
					if (fYPos < oldy)
						if (j <= yblocks + 1)
							alwy = true;
					if (fYPos > oldy)
						if (j >= bufH - 1 + yblocks - 1)
							alwy = true;
					if (yblocks == 0)
						if (j == frameCounter % bufH)
							alwy = true;
					}

					if (frameCounter == 0 || force)
						alwy = true;

					y = (Std.int (fYPos / tileH) + j - 1);
					bufy = Utils.safeMod (y, bufH);

					for (i in 0...bufW)
					{
						alwx = false;

						if (canScrollHorz)
						{
							if (fXPos < oldx)
								if (i <= xblocks + 1)
									alwx = true;
							if (fXPos > oldx)
								if (i >= bufW - 1 + xblocks - 1)
									alwx = true;
							if (xblocks == 0)
								if (i == frameCounter % bufW)
									alwx = true;
						}
						
						x = (Std.int (fXPos / tileW) + i - 1);
						bufx = Utils.safeMod (x, bufW);
						oldtile = mapScreenTiles[bufy][bufx];

						if (! tilesToRedraw.isEmpty())
						{
							for (p in tilesToRedraw)
							if ((p.x == x) && (p.y == y))
							{
								alwx = true;
								tilesToRedraw.remove (p);
							}
						}

						if (alwx || alwy || (oldtile & Def.TF_SEQUENCE != 0))
						{
							pass++;
							getCurTile (x, y);

							if (count < limit || force)
							{
								if ((curTile | curFlags) != oldtile || force)
								{
									m = 0;
									if (curFlags & Def.TF_MIRROR     != 0) m += 1;
									if (curFlags & Def.TF_UPSIDEDOWN != 0) m += 2;

									if (curTile != 0)
										ts.drawTile (surface, bufx * ts.tileW, bufy * ts.tileH, curTile - 1, m);
									else
										if (oldtile != 0)
											surface.fillRect (new Rectangle (bufx * ts.tileW, bufy * ts.tileH, ts.tileW, ts.tileH), 0xffffff);

									oldtile = curTile | curFlags;
									mapScreenTiles[bufy][bufx] = oldtile;

									if (curSeq < 0)  // always update sequences
										count++;
								}
							}
					}

				}
			}

		tilesToRedraw.clear ();

		frameCounter++;

		if (mc01 == null) 
			i = Std.int(fXPos);
		else
			i = Utils.safeMod (Std.int (fXPos), Std.int (bufW * tileW));

		if (mc10 == null)
			j = Std.int(fYPos);
		else
			j = Utils.safeMod (Std.int (fYPos), Std.int (bufH * tileH));

		#if flash8
			mc00._x = -i;
			mc00._y = -j;
		#elseif flash9
			mc00.x = -i;
			mc00.y = -j;
		#end

		if (mc01 != null)
		{
			#if flash8
				mc01._x = -i + bufW * tileW;
				mc01._y = -j;
			#elseif flash9
				mc01.x = -i + bufW * tileW;
				mc01.y = -j;
			#end
		}
		if (mc10 != null)
		{
			#if flash8
				mc10._x = -i;
				mc10._y = -j + bufH * tileH;
			#elseif flash9
				mc10.x = -i;
				mc10.y = -j + bufH * tileH;
			#end
			if (mc11 != null)
			{
				#if flash8
					mc11._x = -i + bufW * tileW;
					mc11._y = -j + bufH * tileH;
				#elseif flash9
					mc11.x = -i + bufW * tileW;
					mc11.y = -j + bufH * tileH;
				#end
			}
		}

	}

	
	public function changeDepth (newMC)
	{
		if (mc00 != null) mcContainer.removeChild (mc00);
		if (mc01 != null) mcContainer.removeChild (mc01);
		if (mc10 != null) mcContainer.removeChild (mc10);
		if (mc11 != null) mcContainer.removeChild (mc11);
		if (lyr != null) mcContainer.removeChild (lyr);
		mcContainer = newMC;
		if (mc00 != null) mcContainer.addChild (mc00);
		if (mc01 != null) mcContainer.addChild (mc01);
		if (mc10 != null) mcContainer.addChild (mc10);
		if (mc11 != null) mcContainer.addChild (mc11);
		if (lyr != null) mcContainer.addChild (lyr);
	}

	
	public static function iterator ()
	{
		return layers.iterator ();
	}
	
}


class EmptyMapInfo extends MapData
{
	public function new ()
	{
		name = "EmptyMap";
		mapW = 1;
		mapH = 1;
	}

	override public function mapdata () : Array<Array<Int>>
	{
		return [[0]];
	}

	override public function boundmapdata () : Array<Array<Int>>
	{
		return [[0]];
	}
}


class SimpleMapInfo extends MapData
{
	public function new ()
	{
		name = "SimpleMap";
		mapW = 1;
		mapH = 1;
	}

	override public function mapdata () : Array<Array<Int>>
	{
		return [[1]];
	}

	override public function boundmapdata () : Array<Array<Int>>
	{
		return [[0]];
	}
}


