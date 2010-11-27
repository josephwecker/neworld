package hesselboom.easing;
class Sine {
	inline private static _HALF_PI:Float = Math.PI / 2;
	
	public static function easeIn (t:Float, b:Float, c:Float, d:Float):Float {
		return -c * Math.cos(t/d * _HALF_PI) + c + b;
	}
	public static function easeOut (t:Float, b:Float, c:Float, d:Float):Float {
		return c * Math.sin(t/d * _HALF_PI) + b;
	}
	public static function easeInOut (t:Float, b:Float, c:Float, d:Float):Float {
		return -c/2 * (Math.cos(Math.PI*t/d) - 1) + b;
	}
}