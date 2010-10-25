package gamelib2d;

class MapData
{
	public var name: String;
	public var mapW: Int;
	public var mapH: Int;

	public function mapdata (): Array<Array<Int>>
	{
		return [[0]];
	}

	public function boundmapdata (): Array<Array<Int>>
	{
		return [[0]];
	}

}
