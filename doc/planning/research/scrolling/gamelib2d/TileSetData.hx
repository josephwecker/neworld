package gamelib2d;

#if flash8
	import flash.MovieClip;
#elseif flash9
	import flash.display.Bitmap;
#end

class TileSetData
{
	public var name: String;
	public var tileW: Int;
	public var tileH: Int;
	public var numTilesX: Int;
	public var numTilesY: Int;
	public var numTiles: Int;
	public var numSequences: Int;

#if flash9
	public function getBitmap (): Bitmap
	{
		return null;
	}
#end

	public function seq (): Array<Array<Int>>
	{
		return [[0]];
	}

}
