package experiments.newexplorer.world;

/* TODO:
 *  - objectpool
 */

class Column {
    var top_layer : Layer;
    public function new(data : Array<flash.display.BitmapData>, px, py) {
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
