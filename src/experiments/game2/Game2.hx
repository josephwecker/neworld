import flash.display.Sprite;
import Terrain;

class Game extends Sprite {
    var t : Terrain;
    var px : Int;
    var py : Int;
    static public function main() {
        haxe.Log.trace = haxeTrace;
        var main_obj = new Game();
    }

    public static function haxeTrace(v: Dynamic, ?infos : Dynamic) {
        flash.Lib.trace("GAME: " + v);
    }

    public function new() {
        super();
        px = 0;
        py = 0;
        t = new Terrain();
        flash.Lib.current.addEventListener(flash.events.Event.ENTER_FRAME,onEnterFrame);
        flash.Lib.current.addEventListener(flash.events.Event.ACTIVATE, onResize);
        flash.Lib.current.addEventListener(flash.events.Event.RESIZE, onResize);
    }
    
    public static function safeDestroy (obj:Dynamic, ?destroy:Bool=true) :Bool {
        if (obj == null) return false;
        var objs :Array<Dynamic> = Std.is (obj, Array) ? obj : [obj];
        for (o in objs) {
            if (o == null) continue;
            if (destroy)    try { o.destroy(); }
                            catch (e:Dynamic) { trace("[Error on object: "+o+", {"+e+"}"); }
            var parent = null; try { parent = o.parent; } catch (e:Dynamic) { trace(e); }
            if (parent != null) parent.removeChild ( o );
        }
        return true;
    }
    
    public static function safeRemove (obj:Dynamic) :Bool {
        return safeDestroy (obj, false);
    }

    function onEnterFrame(e) {
        t.render(px,py);
        px += 1;
        if(px > 8) px = 0;
        var r = Math.random();
        if(r < 0.1) py = (py == 7) ? 0 : py+1;
        else if(r > 0.9) py = (py == 0) ? 7 : py - 1;
        //safeDestroy(t);
    }
}

