package gamelib2d;

class Rnd
{
  static public var seed1: Int;
  static public var seed2: Int;

  static public function getRandomByte (): Int
  {
    seed1 = (seed1 + 0x152 + seed2) << 1;
	seed2 = (seed2 ^ 0x259) + seed1;
	seed1 = ((seed1 << 1) + seed2) & 0xFFFF;
	seed2 = ((seed2 & 0xFF) << 8) + ((seed2 & 0xFF00) >> 8);
	return seed1 & 0xFF;
  }
}

