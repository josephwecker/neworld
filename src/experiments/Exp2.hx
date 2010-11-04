package experiments;

import flash.display.BitmapData;
import flash.display.Bitmap;
import flash.geom.Rectangle;
import flash.geom.Point;
import flash.geom.Matrix;
import RidgedPerlin;
import haxe.Resource;
import flash.display.Shader;
import flash.filters.ShaderFilter;
import flash.utils.ByteArray;

class Exp2 {
    static var maps = new Array<BitmapData>();

    public static function main() {var exp2 = new Exp2();}

    public function new() {
        var mainbmpdat = new BitmapData(1200,600,true,0);
        var src_rect = new Rectangle(0,0,200,200);
        var dst_point= new Point(0,0);
        var seed = Math.ceil(Math.random() * 10);
        var bmd = new BitmapData(200,200,false,0);
        var rp = new RidgedPerlin(seed, 6, 0.7);
        //var rp = new RidgedPerlin();
        //rp.fill(bmd, 0, 0, 0, 0.127, 2.3, 0.6, true);
        rp.fill(bmd, 0, 0, 0, 0.0127 / 2, 2.3, 0.6, true);
        for(y in 0...3) {
            for(i in 0...6) {
                dst_point.x = 200 * i;
                dst_point.y = 200 * y;
                mainbmpdat.copyPixels(bmd,src_rect,dst_point);
            }
        }

        var mainbmp = new Bitmap(mainbmpdat, flash.display.PixelSnapping.AUTO, false);
        var filter_dat : ByteArray = Resource.getBytes('phong').getData();
        var phong : Shader = new Shader(filter_dat);
        var phong_filter = new ShaderFilter(phong);
        mainbmp.filters = [phong_filter];
        flash.Lib.current.addChild(mainbmp);
    }
}
