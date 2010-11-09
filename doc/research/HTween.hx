/**
 * @author Viktor Hesselbom
 */

package hesselboom;
import flash.display.Sprite;
import flash.events.Event;
import flash.Lib;
import haxe.FastList;

class HTween 
{

	public function new (object : Dynamic, duration : Float, properties : Dynamic) 
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
		d = Std.int (duration * 1000);
		start = Lib.getTimer ();
		ease = defaultEase;
		props = new FastList<TweenProp> ();
		for (i in Reflect.fields (properties))
		{
			switch (i)
			{
				case "ease": ease = Reflect.field (properties, i);
				case "onUpdate": onUpdate = Reflect.field (properties, i);
				case "onUpdateParams": onUpdateParams = Reflect.field (properties, i);
				case "onComplete": onComplete = Reflect.field (properties, i);
				case "onCompleteParams": onCompleteParams = Reflect.field (properties, i);
				default:
					if (Std.is (Reflect.field (properties, i), String))
						props.add (new TweenProp (i, Reflect.field (object, i), Std.parseFloat (Reflect.field (properties, i))));
					else
						props.add (new TweenProp (i, Reflect.field (object, i), Reflect.field (properties, i) - Reflect.field (object, i)));
			}
		}
	}
	
	public function remove ()
	{
		rem = true;
	}
	
	public static function to (object : Dynamic, duration : Float, properties : Dynamic)
	{
		return new HTween (object, duration, properties);
	}
	
	public static function from (object : Dynamic, duration : Float, properties : Dynamic)
	{
		for (i in Reflect.fields (properties))
		{
			switch (i)
			{
				case "ease": case "onUpdate": case "onUpdateParams": case "onComplete": case "onCompleteParams": null;
				default:
					var p : Float = Reflect.field (properties, i);
					Reflect.setField (properties, i, Reflect.field (object, i));
					Reflect.setField (object, i, p);
			}
		}
		return new HTween (object, duration, properties);
	}
	
	public static function easeOut (t : Float, b : Float, c : Float, d : Float) : Float
	{
		return -c * (t /= d) * (t - 2) + b;
	}
	
	static function update (e : Event)
	{
		var tween : HTween = first_tween;
		var time : Int = Lib.getTimer ();
		while (tween != null)
		{
			if (tween.rem)
			{
				tween.playing = true;
				if (tween.prev != null) tween.prev.next = tween.next;
				if (tween.next != null) tween.next.prev = tween.prev;
				if (last_tween == tween) last_tween = tween.prev;
				if (first_tween == tween) first_tween = tween.next;
				tween = tween.next;
				continue;
			}
			
			var time_diff : Int = time - tween.start;
			if (time_diff >= tween.d) time_diff = tween.d;
			for (i in tween.props) Reflect.setField (tween.obj, i.field, tween.ease (time_diff, i.initial, i.finished, tween.d));
			
			if (tween.onUpdate != null) tween.onUpdate.apply (null, tween.onUpdateParams);
			
			if (time_diff == tween.d)
			{
				tween.playing = true;
				if (tween.prev != null) tween.prev.next = tween.next;
				if (tween.next != null) tween.next.prev = tween.prev;
				if (last_tween == tween) last_tween = tween.prev;
				if (first_tween == tween) first_tween = tween.next;
				if (tween.onComplete != null) tween.onComplete.apply (null, tween.onCompleteParams);
			}
			
			tween = tween.next;
		}
	}
	
	var rem : Bool;
	var obj : Dynamic;
	var prev : HTween;
	var next : HTween;
	var start : Int;
	var d : Int;
	var props : FastList<TweenProp>;
	public var playing : Bool;
	public var ease : Float -> Float -> Float -> Float -> Float;
	public var onUpdate : Dynamic;
	public var onUpdateParams : Array<Dynamic>;
	public var onComplete : Dynamic;
	public var onCompleteParams : Array<Dynamic>;
	static var first_tween : HTween;
	static var last_tween : HTween;
	static var initiated : Bool;
	static var timer : Sprite;
	public static var defaultEase : Float -> Float -> Float -> Float -> Float = easeOut;
	public static var version : String = "0.2.1";
	
}

class TweenProp
{
	
	public function new (n : String, i : Float, f : Float)
	{
		field = n;
		initial = i;
		finished = f;
	}
	
	public var field : String;
	public var initial : Float;
	public var finished : Float;
	
}
