package experiments.meshexplorer.world;
import experiments.meshexplorer.ViewPort;
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
    public var cross :Bool;

    public var total_height (get_total_height, null) : Int;

    public function new(data : Array<flash.display.BitmapData>, px, py) {
        n = new Array<Column>();

        var next_layer : Layer;
        top_layer =  new Layer(data[0].getPixel(px,py), earth);
        next_layer = top_layer.link_down(new Layer(data[1].getPixel(px,py), air));
        next_layer = next_layer.link_down(new Layer(data[2].getPixel(px,py), earth));
        next_layer = next_layer.link_down(new Layer(data[3].getPixel(px,py), air));
        next_layer.link_down(new Layer(data[4].getPixel(px,py), bedrock));
        
        cross = (((px % 2) == 0) == ((py % 2) == 0));
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
