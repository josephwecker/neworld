import flash.display.Shape;
import flash.Lib;
import hesselboom.easing.Bounce;
import hesselboom.easing.Linear;
import hesselboom.easing.Quad;
import hesselboom.HTween;

class Test 
{
	
	static function main () 
	{
		HTween.to (createBox (0), 2.0, { x : 200.0 } );
		HTween.to (createBox (0), 2.0, { x : 200.0, ease : Quad.easeIn } );
		HTween.to (createBox (0), 2.0, { x : 200.0, ease : Bounce.easeOut } );
		HTween.to (createBox (0), 2.0, { x : 200.0, ease : Bounce.easeIn } );
		HTween.to (createBox (0), 2.0, { x : 200.0, ease : Linear.easeIn } );
		HTween.from (createBox (0), 2.0, { x : 200.0 } );
		HTween.to (createBox (50), 2.0, { x : "100", } );
		
		var b = createBox (0);
		var t : HTween = null;
		var remove_half_way = function (b : Shape)
		{
			if (b.x > 100) t.remove ();
		};
		t = HTween.to (b, 2.0, { x : 200.0, onUpdate : remove_half_way, onUpdateParams : [b] } );
		
		b = createBox (0);
		var reset_when_done = function (b : Shape)
		{
			b.x = 0;
		};
		HTween.to (b, 2.0, { x : 200.0, onComplete : reset_when_done, onCompleteParams : [b] } );
	}
	
	static function createBox (x : Float) : Shape
	{
		var box = new Shape ();
		box.graphics.beginFill (0);
		box.graphics.drawRect (0, h, 20, 20);
		box.x = x;
		Lib.current.addChild (box);
		h += 21.0;
		return box;
	}
	
	static var h = 0.0;
	
}