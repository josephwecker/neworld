package
{
	import flash.display.Shape;
	import flash.display.Sprite;
	import hesselboom.easing.Bounce;
	import hesselboom.easing.Linear;
	import hesselboom.easing.Quad;
	import hesselboom.HTween;

	public class Test extends Sprite
	{
		
		public function Test () 
		{
			HTween.to (createBox (), 2.0, { x : 200.0 } );
			HTween.to (createBox (), 2.0, { x : 200.0, ease : Quad.easeIn } );
			HTween.to (createBox (), 2.0, { x : 200.0, ease : Bounce.easeOut } );
			HTween.to (createBox (), 2.0, { x : 200.0, ease : Bounce.easeIn } );
			HTween.to (createBox (), 2.0, { x : 200.0, ease : Linear.easeIn } );
			
			var b : Shape = createBox ();
			var t : HTween = null;
			var remove_half_way : Function = function (b : Shape) : void
			{
				if (b.x > 100) t.remove ();
			};
			t = HTween.to (b, 2.0, { x : 200.0, onUpdate : remove_half_way, onUpdateParams : [b] } );
			
			b = createBox ();
			var reset_when_done : Function = function (b : Shape) : void
			{
				b.x = 0;
			};
			HTween.to (b, 2.0, { x : 200.0, onComplete : reset_when_done, onCompleteParams : [b] } );
		}
		
		private function createBox () : Shape
		{
			var box : Shape = new Shape ();
			box.graphics.beginFill (0);
			box.graphics.drawRect (0, h, 20, 20);
			addChild (box);
			h += 21.0;
			return box;
		}
		
		private var h : Number = 0.0;
		
	}
}