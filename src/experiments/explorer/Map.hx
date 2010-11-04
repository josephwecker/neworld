package experiments.explorer;

import RidgedPerlin;
import flash.display.BitmapData;
import neworld.Utils;
//using  Utils;

class Map {
    public var layers : Array<BitmapData>;
    var seed : Int;

    static var S_CORE = new H<Dynamic>([
        ['persistance', 0.55],
        ['octaves',     15],
        ['base_factor', 0.027],
        //['base_factor', 0.00635],
        //['gain',        2.3],
        ['gain',        1.0],
        ['offset',      0.68]]);

    static var S_AIR = new H<Dynamic>([
        ['persistance', 0.7],
        ['octaves',     10],
        ['base_factor', 0.02],
        //['base_factor', 0.00635],
        //['gain',        2.3],
        ['gain',        2.6],
        ['offset',      0.5]]);

    public function new(?seed) {
        if(seed == null) seed = Math.floor(Math.random() * 0xffffff);
        this.seed = seed;
        layers = new Array();
        construct_node();
    }

    function next_seed() {
        seed += 1;
        return seed;
    }

    function construct_node() {
        layers.push(core_layer());
        for(i in 0...5) {
            layers.push(air_layer());
            layers.push(earth_layer());
        }
    }

    function core_layer() {
        var rp = new RidgedPerlin(next_seed(), S_CORE.octaves, S_CORE.persistance);
        var layer_data = new BitmapData(64,128,false,0);
        rp.fill(layer_data, U.rand(100), U.rand(100), U.rand(100),
            S_CORE.base_factor, S_CORE.gain, S_CORE.offset, true);
        return smooth_edges(layer_data);
    }

    function air_layer() {
        return new BitmapData(64,128,false,0);
    }

    function earth_layer() {
        return new BitmapData(64,128,false,0);
    }

    function smooth_edges(layer : BitmapData) {
        var shape = new flash.display.Shape();
        var matrix = new flash.geom.Matrix();
        matrix.createGradientBox(64,128,0,0,0);
        shape.graphics.beginGradientFill(flash.display.GradientType.RADIAL,
            [0xFFFFFF, 0, 0], [0.2, 0.2, 1], [0, 200, 255], matrix,
            flash.display.SpreadMethod.PAD,
            flash.display.InterpolationMethod.RGB, 0);
        shape.graphics.drawRect(0, 0, 64, 128);
        shape.graphics.endFill();
        layer.draw(shape);
        return layer;
    }
}

/*class Node {
    public function new() {}
}*/

class Column {
    public function new() {}
}

class Location {
    //var node : Node;
    public var column : Column;
    public var height : UInt;
    public var hot_percent_x : Float;
    public var hot_percent_y : Float;
}
