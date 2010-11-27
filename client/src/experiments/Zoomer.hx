package experiments;
import flash.display.BitmapData;
import flash.display.Bitmap;
import flash.geom.Rectangle;
import flash.geom.Point;
import flash.geom.Matrix;
import noise.RidgedPerlin;
import de.polygonal.core.event.IObserver;
import de.polygonal.core.event.Observable;
import de.polygonal.core.time.Timebase;
import de.polygonal.core.time.TimebaseEvent;

class Zoomer extends flash.display.Sprite, implements IObserver {
    var mainbmpdat : BitmapData;
    var mainbmp : Bitmap;
    var rp : RidgedPerlin;
    var seed : Int;

    var base : Float;
    var gain : Float;
    var offset : Float;
    var persistence : Float;
    var octaves : Int;

    var scaler  :Int;
    var point_x :Float;
    var point_y :Float;

    public var key :Int;

    public static function main() {
       haxe.Log.trace = function(v:Dynamic,?i){flash.Lib.trace("Zoomer: "+v);};
       var zoomer = new Zoomer();
    }

    public function new() {
        super();
        key = 0;
        flash.Lib.current.cacheAsBitmap =   true;
        flash.Lib.current.stage.scaleMode =                   flash.display.StageScaleMode.NO_SCALE;
        flash.Lib.current.stage.align =                       flash.display.StageAlign.TOP_LEFT;
        flash.Lib.current.stage.showDefaultContextMenu=       false;
        flash.Lib.current.contextMenu =     new flash.ui.ContextMenu();
        flash.Lib.current.contextMenu.hideBuiltInItems();
        flash.Lib.current.mouseEnabled =    false;

        // Keep as power of 2!
        base =        0.002;
        gain =        1.0;//1.0005;
        offset =      0.735;
        octaves =     18;
        persistence = 0.9;

        point_x = 300 + 90;
        point_y = 300 + 90;

        mainbmpdat = new BitmapData(256,512,false,0);
        scaler =     0;
        rp =         new RidgedPerlin(156, octaves, persistence);
        mainbmp =    new Bitmap(mainbmpdat, flash.display.PixelSnapping.ALWAYS, false);

        //update(0, null, null);
        do_update(null);
        flash.Lib.current.addChild(mainbmp);
        flash.Lib.current.stage.addEventListener(flash.events.KeyboardEvent.KEY_DOWN, do_update);
        //Timebase.attach(this, TimebaseEvent.RENDER);
    }

    public function update(type : Int, source : Observable, data : Dynamic) {
        do_update(null);
    }

    public function do_update(_) {
        mainbmpdat.lock();
        //var real_base = base * (1 / (scaler * scaler * scaler));
        //rp.fill(mainbmpdat, 0, 0, 0, real_base, gain, offset, true);
        rp.fill(mainbmpdat,
                point_x * (1 << scaler),
                point_y * (1 << scaler),
                0,
                base / (1 << scaler),
                gain,
                offset,
                true, 0);

        mainbmpdat.unlock();
        trace("SCALE FACTOR: "+scaler);
        //var incby = 128.0 / real_base;   // SHOWS INFINITE LANDSCAPE
        scaler += 2;
    }
}
