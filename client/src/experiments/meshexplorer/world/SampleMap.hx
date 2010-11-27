package experiments.meshexplorer.world;

import noise.RidgedPerlin;
import flash.display.BitmapData;

class SampleMap {
    /*
    static var BASE =        0.002;
    static var GAIN =        1.25;
    static var OFFSET =      0.7;
    static var OCTAVES =     7;
    static var PERSISTENCE = 0.9;
    static var SEED =        156;
    static var X =           300;
    static var Y =           300;
    */

    static var BASE =        0.002 / (1 << 4);
    static var GAIN =        1.0;
    static var OFFSET =      0.735;
    static var OCTAVES =     4;
    static var PERSISTENCE = 0.9;
    static var SEED =        156;
    static var X =           390 * (1 << 4);
    static var Y =           390 * (1 << 4);

    var rp : RidgedPerlin;

    public var data : Array<BitmapData>;

    public function new() {
        rp = new RidgedPerlin(SEED, OCTAVES, PERSISTENCE);
        data = new Array<BitmapData>();
        fill_maps();
    }

    function fill_maps() {
        var bmd = new BitmapData(100, 100, false, 0);
        rp.fill(bmd, X, Y, 0, BASE, GAIN, OFFSET, true);
        data.push(bmd);  // Base bedrock height

        for(i in 1...3) {
            bmd = new BitmapData(100, 100, false, 0);
            rp.fill(bmd, X, Y, 0, BASE + (i / 100), GAIN, OFFSET - (i / 5), true);
            data.push(bmd);
            bmd = new BitmapData(100, 100, false, 0);
            rp.fill(bmd, X, Y, 0, BASE + (i / 75), GAIN, OFFSET - (i / 7.5), true);
            data.push(bmd);
        }

        data.reverse();  // Top-most first
    }
}
