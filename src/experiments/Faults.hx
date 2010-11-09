package experiments;

class Faults {
    var fault_noise :FaultNoise;
    var main_bmp    :flash.display.Bitmap;
    var bmp_dat     :flash.display.BitmapData;

    public static function main() {
        var main_fault = new Faults(400);
    }

    public function new(size) {
        bmp_dat = new flash.display.BitmapData(size, size, false, 0x808080);
        fault_noise = new FaultNoise(size, 10);
        fault_noise.fill(bmp_dat);
        main_bmp = new flash.display.Bitmap(bmp_dat);
        flash.Lib.current.addChild(main_bmp);
    }
}

class FaultNoise {
    
    var size            :Int;
    var iterations      :Int;
    var line_slopes     :Array<Float>;
    var line_depths     :Array<Array<Int>>;
    var line_coords     :Array<Array<Float>>;

    public function new(size, iterations) {
        this.size = size;
        this.iterations = iterations;

        var rise        :Int;
        var drop        :Int;
        var degrees     :Float;
        var x_offset    :Float;
        var y_offset    :Float;

        for( count in 0...iterations ) {
            line_slopes = [];
            line_depths = [];
            line_coords = [];
            rise     = 6;//Math.floor(Math.random()*(count*.5));
            drop     = 4;//Math.floor(Math.random()*(count*.5));
            degrees  = Math.random()*360;
            x_offset    = Math.cos(degrees);
            y_offset    = Math.sin(degrees);
            line_depths.push([rise,drop]);
            line_coords.push([Math.random()*400, Math.random()*400]);
            if( x_offset != 0 ) {
                line_slopes.push(y_offset/x_offset);
            } else {
                line_slopes.push(999999);
            }
        }
    }
    
    
    public function fill(data) {
        //var slope       :Float;
        //var rise        :Int;
        //var drop        :Int;
        var x_intercept   :Float;
        for( i in 0...iterations ) {
            var slope = line_slopes[i];
            //var rise = 6;
            trace(line_depths[i][0]);
            //var rise  = line_depths[i][0];
            //var drop  = line_depths[i][1];
            for( x in 0...size) {
                //x_intercept = ( (x - line_coords[i][1]) / slope) + line_coords[i][0];
                //if( x_intercept < 0 ) { x_intercept = 0; }
                //if( x_intercept > size) { x_intercept = size; }
                for( y in 0...size) {
                    //var new_color = data.getPixel(x,y) - ((rise << 16) + (rise << 8) + (rise));
                    data.setPixel(x,y,0x009900);
                }
            }
        }
    }
}

