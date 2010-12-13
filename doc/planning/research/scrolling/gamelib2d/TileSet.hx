package gamelib2d;

import gamelib2d.Utils;
import gamelib2d.Def;

#if flash8
	import flash.MovieClip;
#elseif flash9
	import flash.display.MovieClip;
	import flash.display.Sprite;
	import flash.display.Bitmap;
#end
import flash.display.BitmapData;
import flash.geom.Point;
import flash.geom.Rectangle;


class TileSet
{
	static var tilesets: List<TileSet> = new List ();
	
#if flash8
	private var mcContainer: MovieClip;
	private var mcLoader: MovieClip;
#elseif flash9
	private var mcContainer: Sprite;
	private var mcLoader: Bitmap;
#end

	private var bitmap: Array<BitmapData>;

	public var name: String;
	public var tileW: Int;
	public var tileH: Int;
	public var numTilesX: Int;
	public var numTilesY: Int;
	public var numTiles: Int;

	public var seq: Array<Array<Int>>;
	public var seqCurFrame: Array<Int>;
	public var seqCurFrameCounter: Array<Int>;
	public var seqTotalFrames: Array<Int>;
	public var numSequences: Int;
	
	public var hasTransparency: Array<Bool>;
	

	public function new (mc)
	{
		mcContainer = mc;
		tilesets.add (this);
	}

	public function clear ()
	{
		tilesets.remove (this);
		if (mcContainer != null && mcLoader != null) 
			mcContainer.removeChild (mcLoader);
		mcLoader = null;
		if (bitmap != null)
		{
			bitmap[0].dispose ();
			bitmap[1].dispose ();
			bitmap[2].dispose ();
			bitmap[3].dispose ();
		}
		bitmap = null;
		seq = null;
		seqCurFrame = null;
		seqCurFrameCounter = null;
		seqTotalFrames = null;
	}

	
	public function createFromBitmapData (bd: BitmapData, ?tileWidth: Int = 0, ?tileHeight: Int = 0)
	{
		if (tileWidth * tileHeight == 0)
		{
			tileW = bd.width;
			tileH = bd.height;
			numTilesX = 1;
			numTilesY = 1;
			numTiles = 1;
		}
		else
		{
			tileW = tileWidth;
			tileH = tileHeight;
			numTilesX = Std.int ((bd.width + tileW - 1) / tileW);
			numTilesY = Std.int ((bd.height + tileH - 1) / tileH);
			numTiles = numTilesX * numTilesY;
		}
		bitmap = new Array ();
		bitmap[0] = bd;
		bitmap[1] = bd;
		bitmap[2] = bd;
		bitmap[3] = bd;
		
		numSequences = 0;
		seq = [];
		//seqCurFrame = new Array ();
		//seqCurFrameCounter = new Array ();
		//seqTotalFrames = new Array ();
	}
	
	
	public function init (data: TileSetData)
	{
		#if flash8
			var r: Rectangle<Int>;
		#elseif flash9
			var r: Rectangle;
		#end

		name = data.name;
		tileW = data.tileW;
		tileH = data.tileH;
		numTilesX = data.numTilesX;
		numTilesY = data.numTilesY;
		numTiles = data.numTiles;

		bitmap = new Array ();
		
		hasTransparency = new Array ();

		#if flash9
			mcLoader = data.getBitmap ();

			// these are for mirrored and upsidedown copies of the tileset
			bitmap[0] = new BitmapData (Std.int (mcLoader.width), Std.int (mcLoader.height), true, 0x0);
			bitmap[1] = new BitmapData (Std.int (mcLoader.width), Std.int (mcLoader.height), true, 0x0);
			bitmap[2] = new BitmapData (Std.int (mcLoader.width), Std.int (mcLoader.height), true, 0x0);
			bitmap[3] = new BitmapData (Std.int (mcLoader.width), Std.int (mcLoader.height), true, 0x0);
		#elseif flash8
			mcLoader = mcContainer.attachMovie (name, "mcLoader", flash.Lib._root.getNextHighestDepth ());

			bitmap[0] = new BitmapData (Std.int (mcLoader._width), Std.int (mcLoader._height), true, 0x0);
			bitmap[1] = new BitmapData (Std.int (mcLoader._width), Std.int (mcLoader._height), true, 0x0);
			bitmap[2] = new BitmapData (Std.int (mcLoader._width), Std.int (mcLoader._height), true, 0x0);
			bitmap[3] = new BitmapData (Std.int (mcLoader._width), Std.int (mcLoader._height), true, 0x0);
		#end

		bitmap[0].draw (mcLoader);
		//bitmap[1].draw (mcLoader);
		//bitmap[2].draw (mcLoader);
		//bitmap[3].draw (mcLoader);
		for (k in 0...numTiles)
		{
			r = getTileRect (k);
			for (i in 0...tileW)
				bitmap[1].copyPixels (bitmap[0], new Rectangle (r.x + tileW - 1 - i, r.y, 1, tileH), new Point (r.x + i, r.y));
			for (j in 0...tileH)
				bitmap[2].copyPixels (bitmap[0], new Rectangle (r.x, r.y + tileH - 1 - j, tileW, 1), new Point (r.x, r.y + j));
			for (i in 0...tileW)
				bitmap[3].copyPixels (bitmap[2], new Rectangle (r.x + tileW - 1 - i, r.y, 1, tileH), new Point (r.x + i, r.y));
				
			hasTransparency[k] = false;
			for (i in 0...tileW)
				for (j in 0...tileH)
					if (bitmap[0].getPixel32 (Std.int (r.x) + i, Std.int (r.y) + j) & 0xFF000000 != 0xFF000000)    // fixed, was:  == 0
						hasTransparency[k] = true;
		}

		#if flash9
			mcLoader = null;
		#elseif flash8
			mcLoader.removeMovieClip ();
		#end

		numSequences = data.numSequences;
		seq = data.seq ();
		seqCurFrame = new Array ();
		seqCurFrameCounter = new Array ();
		seqTotalFrames = new Array ();

		for (i in 0...numSequences)
		{
			seqCurFrame[i] = -1;
			seqCurFrameCounter[i] = 0;

			seqTotalFrames[i] = 0;
			for (j in 0...seq[i].length)
				if (j % 3 == 1)
					seqTotalFrames[i] += seq[i][j] + 1;
		}
	}
	
	public function editPixels (f: BitmapData -> Rectangle -> BitmapData, ?frame: Int)
	{
		var bd: BitmapData;
		var r: Rectangle;
		if (frame == null) frame = -1;
		for (l in 0...4)
		{
			for (k in 0...numTiles)
				if ((k == frame) || (frame == -1))
				{
					r = getTileRect (k);
					bd = new BitmapData (tileW, tileH, true, 0);
					bd.copyPixels (bitmap[l], r, new Point (0, 0));
					bd = f (bd, new Rectangle (0, 0, tileW, tileH));
					bitmap[l].copyPixels (bd, new Rectangle (0, 0, tileW, tileH), new Point (r.x, r.y));
				}
		}
	}

	public function getTileRect (n: Int)
	{
		var tileX: Int = Utils.safeMod (n, numTilesX);
		var tileY: Int = Std.int (n / numTilesX);
		return new Rectangle (tileX * tileW, tileY * tileH, tileW, tileH);
	}

	public function drawTile (surface: BitmapData, x: Int, y: Int, n: Int, m: Int)
	{
		surface.copyPixels (bitmap[m], getTileRect (n), new Point (x, y));
	}
	
	public function getTileBD (n: Int, ?m: Int = 0): BitmapData
	{
		var bd: BitmapData = new BitmapData (tileW, tileH, true, 0);
		if (n < 0) n = getAnimationFrame (( -n) - 1, 0) - 1;
		drawTile (bd, 0, 0, n, m);
		return bd;
	}

	public function runSequences ()
	{
		if (numSequences > 0)
			for (i in 0...numSequences)
				if (--seqCurFrameCounter[i] < 0)
				{
					seqCurFrame[i]++;
					if (3 * seqCurFrame[i] >= seq[i].length)
						seqCurFrame[i] = 0;
					seqCurFrameCounter[i] = seq[i][3 * seqCurFrame[i] + 1];
				}
	}

	public function GetSequenceFrame (n: Int)
	{
		return seq[n][3 * seqCurFrame[n]];
	}

	public function GetSequenceFrameBounds (n: Int)
	{
		return seq[n][3 * seqCurFrame[n] + 2];
	}

	public function getAnimationFrameCount (n: Int)
	{
		return seqTotalFrames[n];
	}
  
	public function getAnimationFrame (n: Int, time: Int)	// n = 0, 1, 2, ...
	{
		var tile: Int = 1;
		var i = time % seqTotalFrames[n];
		
		if (n >= numSequences)
			return n + 1;  // bugfix
		while ((i > seq[n][tile]) && (tile + 3 < seq[n].length))
		{
			i -= seq[n][tile] + 1;
			tile += 3;
		}
		// bnds = Seq[n][tile + 1]
		return seq[n][tile - 1];
	}

	public static function iterator ()
	{
		return tilesets.iterator ();
	}
	
}
