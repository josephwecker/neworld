import flash.display.BitmapData;
import flash.display.Bitmap;
import flash.geom.Rectangle;
import flash.geom.Point;
import flash.geom.Matrix;
import RidgedPerlin;
//import haxe.Resource;
//import flash.display.Shader;
//import flash.filters.ShaderFilter;
//import flash.utils.ByteArray;

class Exp2 {
    static var maps = new Array<BitmapData>();

    public static function main() {
       haxe.Log.trace = function(v:Dynamic,?i){flash.Lib.trace("Explorer: "+v);};
       var exp2 = new Exp2();
    }

    public function new() {
        var mainbmpdat = new BitmapData(1200,600,true,0);
        var src_rect = new Rectangle(0,0,600,600);
        var dst_point= new Point(0,0);
        var seed = Math.ceil(Math.random() * 10);
        var bmd = new BitmapData(600,600,false,0);
        //var rp = new RidgedPerlin(seed, 13, 0.7);
        var rp = new RidgedPerlin(seed, 13, 0.7);
        rp.fill(bmd, 0, 0, 0, 0.0127/3, 2.3, 0.6, true);
        //for(y in 0...2) {
            for(i in 0...2) {
                dst_point.x = 600 * i;
                //dst_point.y = 600 * y;
                mainbmpdat.copyPixels(bmd,src_rect,dst_point);
                var mainbmp = new Bitmap(mainbmpdat, flash.display.PixelSnapping.AUTO, false);
                flash.Lib.current.addChild(mainbmp);
            }
        //}

        //var filter_dat : ByteArray = Resource.getBytes('phong').getData();
        //var phong : Shader = new Shader(filter_dat);
        //var phong_filter = new ShaderFilter(phong);
        //mainbmp.filters = [phong_filter];
    }
}
