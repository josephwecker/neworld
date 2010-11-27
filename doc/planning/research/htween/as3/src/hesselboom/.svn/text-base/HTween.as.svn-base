/**
 * @author Viktor Hesselbom
 */

package hesselboom
{
	import flash.display.Sprite;
	import flash.events.Event;
	import flash.utils.getTimer;

	public class HTween 
	{
		
		public function HTween (object : Object, duration : Number, properties : Object) 
		{
			if (!initiated)
			{
				timer = new Sprite ();
				timer.addEventListener (Event.ENTER_FRAME, update, false, 0, true);
			}
			
			if (first_tween == null) first_tween = this;
			prev = last_tween;
			if (prev != null) prev.next = this;
			last_tween = this;
			
			playing = true;
			obj = object;
			d = int (duration * 1000);
			start = getTimer ();
			ease = defaultEase;
			props = new Array ();
			for (var i : String in properties)
			{
				switch (i)
				{
					case "ease": ease = properties [i]; break;
					case "onUpdate": onUpdate = properties [i]; break;
					case "onUpdateParams": onUpdateParams = properties [i]; break;
					case "onComplete": onComplete = properties [i]; break;
					case "onCompleteParams": onCompleteParams = properties [i]; break;
					default:
						if (properties [i] is String)
							props.push (new TweenProp (i, object [i], Number (properties [i]))); break;
						else
							props.push (new TweenProp (i, object [i], properties [i] - object [i])); break;
				}
			}
		}
		
		public function remove () : void
		{
			rem = true;
		}
		
		public static function to (object : Object, duration : Number, properties : Object) : HTween
		{
			return new HTween (object, duration, properties);
		}
		
		public static function easeOut (t : Number, b : Number, c : Number, d : Number) : Number
		{
			return -c * (t /= d) * (t - 2) + b;
		}
		
		private static function update (e : Event) : void
		{
			var tween : HTween = first_tween;
			var time : int = getTimer ();
			while (tween != null)
			{
				if (tween.rem)
				{
					tween.playing = false;
					if (tween.prev != null) tween.prev.next = tween.next;
					if (tween.next != null) tween.next.prev = tween.prev;
					if (last_tween == tween) last_tween = tween.prev;
					if (first_tween == tween) first_tween = tween.next;
					tween = tween.next;
					continue;
				}
				
				var time_diff : int = time - tween.start;
				if (time_diff >= tween.d) time_diff = tween.d;
				for each (var i : TweenProp in tween.props) tween.obj [i.field] = tween.ease (time_diff, i.initial, i.finished, tween.d);
				
				if (tween.onUpdate != null) tween.onUpdate.apply (null, tween.onUpdateParams);
				
				if (time_diff == tween.d)
				{
					tween.playing = false;
					if (tween.prev != null) tween.prev.next = tween.next;
					if (tween.next != null) tween.next.prev = tween.prev;
					if (last_tween == tween) last_tween = tween.prev;
					if (first_tween == tween) first_tween = tween.next;
					if (tween.onComplete != null) tween.onComplete.apply (null, tween.onCompleteParams);
				}
				
				tween = tween.next;
			}
		}
		
		private var rem : Boolean;
		private var obj : Object;
		private var prev : HTween;
		private var next : HTween;
		private var start : int;
		private var d : int;
		private var props : Array;
		public var playing : Boolean;
		public var ease : Function;
		public var onUpdate : Function;
		public var onUpdateParams : Array;
		public var onComplete : Function;
		public var onCompleteParams : Array;
		private static var first_tween : HTween;
		private static var last_tween : HTween;
		private static var initiated : Boolean;
		private static var timer : Sprite;
		public static var defaultEase : Function = easeOut;
		public static var version : String = "0.2.1";
		
	}
}

class TweenProp
{
	
	public function TweenProp (n : String, i : Number, f : Number)
	{
		field = n;
		initial = i;
		finished = f;
	}
	
	public var field : String;
	public var initial : Number;
	public var finished : Number;
	
}