import flash.display.BitmapData;
import flash.display.Bitmap;
import flash.geom.Rectangle;
import flash.geom.Point;
import flash.geom.Matrix;
import RidgedPerlin;
import de.polygonal.core.event.IObserver;
import de.polygonal.core.event.Observable;
import de.polygonal.core.time.Timebase;
import de.polygonal.core.time.TimebaseEvent;

class Zoomer extends flash.display.Sprite { //, implements IObserver {
    var mainbmpdat : BitmapData;
    var mainbmp : Bitmap;
    var rp : RidgedPerlin;
    var scaler : Float;
    var seed : Int;

    var base : Float;
    var gain : Float;
    var offset : Float;
    var persistence : Float;
    var octaves : Int;

    public static function main() {
       haxe.Log.trace = function(v:Dynamic,?i){flash.Lib.trace("Zoomer: "+v);};
       var zoomer = new Zoomer();
    }

    public function new() {
        super();
        //flash.Lib.current.addChild(this);
        //flash.Lib.current.cacheAsBitmap =   true;
        //flash.Lib.current.stage.scaleMode =                   flash.display.StageScaleMode.NO_SCALE;
        //flash.Lib.current.stage.align =                       flash.display.StageAlign.TOP_LEFT;
        //flash.Lib.current.stage.showDefaultContextMenu=       false;
        //flash.Lib.current.contextMenu =     new flash.ui.ContextMenu();
        //flash.Lib.current.contextMenu.hideBuiltInItems();
        //flash.Lib.current.mouseEnabled =    false;

        base = 0.008;
        gain = 1.25;
        offset = 0.7;
        octaves = 7;
        persistence = 0.9;

        mainbmpdat = new BitmapData(400,200,false,0);
        //scaler =     0xfffeffff;
        scaler = 1;
        rp =         new RidgedPerlin(Math.ceil(Math.random() * 100), octaves, persistence);
        mainbmp =    new Bitmap(mainbmpdat, flash.display.PixelSnapping.ALWAYS, false);

        do_update(null);
        flash.Lib.current.addChild(mainbmp);
        flash.Lib.current.stage.addEventListener(flash.events.KeyboardEvent.KEY_DOWN, do_update);
        //Timebase.attach(this, TimebaseEvent.RENDER);
    }

    //public function update(type : Int, source : Observable, data : Dynamic) {
    public function do_update(_) {
        mainbmpdat.lock();
        var real_base = base * (1 / scaler);
        rp.fill(mainbmpdat, ((scaler+1) * (400 / 2)), ((scaler+1) * (200/2)), 0, real_base, gain, offset, true);
        mainbmpdat.unlock();
        //var incby = 128 / base;   // SHOWS INFINITE LANDSCAPE
        //scaler += incby;
        scaler += 1;
        trace(scaler);
    }
}