package experiments;
import Simplex;
import haxe.Timer;

class TestSimplex {
    static public function main() {
        haxe.Log.trace = function(v:Dynamic,?i){flash.Lib.trace("STest: "+v);};
        var stest = new TestSimplex();
    }

    public function new() {
        flash.Lib.current.cacheAsBitmap =   true;
        flash.Lib.current.stage.scaleMode = flash.display.StageScaleMode.NO_SCALE;
        flash.Lib.current.stage.align =     flash.display.StageAlign.TOP_LEFT;
        trace("start\n");
        var time = Timer.stamp();
        var seed = Math.floor(Math.random() * 1000);
        var bmd = new flash.display.BitmapData(500,500);

        var noise = new SimplexNoise(seed, 20, 0.7);
        noise.noise2D(bmd);
        //var noise = new OptimizedPerlin(seed, 20, 0.7);
        //noise.fill(bmd);

        var bm = new flash.display.Bitmap(bmd);
        flash.Lib.current.addChild(bm);
        time = Timer.stamp() - time;
        trace("end ("+time+")\n");
    }
}
