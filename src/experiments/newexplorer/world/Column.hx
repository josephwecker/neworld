package experiments.newexplorer.world;
import experiments.newexplorer.ViewPort;
import haxe.macro.Expr;
import haxe.macro.Context;

/* TODO:
 *  - objectpool
 *  - Actually have the columns create themselves based on the main algorithm,
 *    lazily as needed.  Basically change the 'n' array into a property and if
 *    any of the directions are null it builds it on the fly to return it.
 *    Therefore it really is infinite.  Bonus: also have it expire something on
 *    the other side as needed (or possibly watch access for them / garbage
 *    collect).
 */

class Column {
    var top_layer : Layer;
    public var n : Array<Column>;

    public var total_height (get_total_height, null) : Int;

    public function new(data : Array<flash.display.BitmapData>, px, py) {
        n = new Array<Column>();

        var next_layer : Layer;
        top_layer =  new Layer(data[0].getPixel(px,py), earth);
        next_layer = top_layer.link_down(new Layer(data[1].getPixel(px,py), air));
        next_layer = next_layer.link_down(new Layer(data[2].getPixel(px,py), earth));
        next_layer = next_layer.link_down(new Layer(data[3].getPixel(px,py), air));
        next_layer.link_down(new Layer(data[4].getPixel(px,py), bedrock));
    }

    public function describe() {
        var ret = '';
        var layer = top_layer;
        while(layer != null) {
            ret += 'Layer of air\n';
            ret += 'Layer of '+layer.substance+' ('+layer.height+' meters)\n';
            layer = layer.next_down;
        }
        return ret;
    }

    function get_total_height() : Int {
        var layer = top_layer;
        var res = 0;
        while(layer != null) {
            res += layer.height;
            layer = layer.next_down;
        }
        return res;
    }

    public function move_by(dir, times) {
        var r = this;
        for(i in 0...times) r = r.n[D.rel(dir)];
        return r;
    }

    /*public function up_by  (dir:O,n:Int){var r=this;for(i in 0...n)r=r.up(dir);  return r;}
    public function up_by  (dir:O,n:Int){var r=this;for(i in 0...n)r=r.up(dir);  return r;}
    public function left_by(dir:O,n:Int){var r=this;for(i in 0...n)r=r.left(dir);return r;}

    public function up_by        (dir,n){return do_times('up',        dir,n);}
    public function up_right_by  (dir,n){return do_times('up_right',  dir,n);}
    public function right_by     (dir,n){return do_times('right',     dir,n);}
    public function down_right_by(dir,n){return do_times('down_right',dir,n);}
    public function down_by      (dir,n){return do_times('down',      dir,n);}
    public function down_left_by (dir,n){return do_times('down_left', dir,n);}
    public function left_by      (dir,n){return do_times('left',      dir,n);}
    public function up_left_by   (dir,n){return do_times('up_left',   dir,n);}

    public inline function up        (dir:Orientation){return n_rot(north,    dir);}
    public inline function right     (dir){return n_rot(east,     dir);}
    public inline function down      (dir){return n_rot(south,    dir);}
    public inline function left      (dir){return n_rot(west,     dir);}
    public inline function up_right  (dir){return n_rot(northeast,dir);}
    public inline function down_right(dir){return n_rot(southeast,dir);}
    public inline function down_left (dir){return n_rot(southwest,dir);}
    public inline function up_left   (dir){return n_rot(northwest,dir);}

    // Neighbor on main_dir side, but rotated by something
    public inline function n_rot(main_dir :Orientation, rotate_by :Orientation) {
        return n[(Dir.i(rotate_by) + Dir.i(main_dir)) % 8];
    }

    inline function do_times(fun_name :String, dir :Orientation, times :Int) {
        var res = this;
        var func :Orientation->Column;
        //for(i in 0...times) res = Reflect.callMethod(res, func, [dir]);
        // TODO: CHANGE ORIENTATION FROM ENUM INTO STATIC CONSTANTS
        for(i in 0...times) {
            func = Reflect.field(res, fun_name);
            res = func(cast(dir, Orientation));
        }
        return res;
    }*/

}

class Layer {
    public var substance : TerrainSubstance;
    public var height    : UInt;
    public var next_up   : Layer;
    public var next_down : Layer;

    public function new(height, substance) {
        this.height = height;
        this.substance = substance;
        next_up = null;
        next_down = null;
    }

    public function link_down(next) {
        next_down = next;
        next.next_up = this;
        return next;
    }
}

enum TerrainSubstance {
    bedrock;
    air;
    earth;
}
