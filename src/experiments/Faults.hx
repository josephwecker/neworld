package experiments;

import flash.events.KeyboardEvent;

class Faults {
    var fault_noise :FaultNoise;
    var main_bmp    :flash.display.Bitmap;
    var bmp_dat     :flash.display.BitmapData;

    public static function main() {
        haxe.Log.trace = function(v:Dynamic,?i){flash.Lib.trace("Faultline: "+v);};
        flash.Lib.current.cacheAsBitmap =   true;
        flash.Lib.current.stage.scaleMode =                   flash.display.StageScaleMode.NO_SCALE;
        flash.Lib.current.stage.align =                       flash.display.StageAlign.TOP_LEFT;
        flash.Lib.current.stage.showDefaultContextMenu=       false;
        flash.Lib.current.contextMenu =     new flash.ui.ContextMenu();
        flash.Lib.current.contextMenu.hideBuiltInItems();
        flash.Lib.current.mouseEnabled =    false;
        var main_fault = new Faults(400);
    }

    public function new(size) {
        bmp_dat = new flash.display.BitmapData(size, size, false, 0x808080);
        fault_noise = new FaultNoise(size, 8);
        flash.Lib.current.stage.addEventListener(KeyboardEvent.KEY_DOWN, iterate_once);
        main_bmp = new flash.display.Bitmap(bmp_dat);
        flash.Lib.current.addChild(main_bmp);
    }

    function iterate_once(event:KeyboardEvent) {
        fault_noise.fill(bmp_dat);
    }
}

class FaultNoise {
    
    var size            :Int;
    var octaves         :Int;
    var line_slopes     :Array<Float>;
    var line_depths     :Array<Array<Int>>;
    var line_coords     :Array<Array<Float>>;

    var current_i       :Int;
    var current_o       :Int;

    public function new(size, octaves) {
        this.size = size;
        this.octaves = octaves;
        current_i = 0;
        current_o = 1;

        var rise        :Int;
        var drop        :Int;
        var degrees     :Float;
        var x_offset    :Float;
        var y_offset    :Float;
            
        line_slopes = [];
        line_depths = [];
        line_coords = [];

        for( o in 1...octaves ) {
            rise     = Math.floor(128/(o*o+1));
            drop     =  rise;//Math.floor(Math.random()*128/(count+1.0*4));
            trace(rise);
            line_depths.push([rise,-drop]);
            for( count in 0...octaves*2 ) {
                degrees  = Math.random()*360;
                x_offset    = Math.cos(degrees);
                y_offset    = Math.sin(degrees);
                line_coords.push([Math.random()*400, Math.random()*400]);
                if( x_offset != 0 ) {
                    line_slopes.push(y_offset/x_offset);
               } else {
                    line_slopes.push(999999);
                }
            }
        }
    }
    
    
    public function fill(data:flash.display.BitmapData) {
        //var slope       :Float;
        //var rise        :Int;
        //var drop        :Int;
        data.lock();
        var x_intercept   :Float;
        for( i in 0...(current_o*2) ) {
            var slope = line_slopes[current_i];
            var rise :Int = Math.ceil(Math.random()*line_depths[current_o+1][0]);
            var drop :Int = Math.ceil(Math.random()*line_depths[current_o+1][1]);
            trace(rise);
            if(Math.floor(Math.random()*2) == 1) {
                var d = rise; rise = drop; drop = d;
            }
            for( x in 0...size) {
                x_intercept = ( (x - line_coords[current_i][1]) / slope) +
                    line_coords[current_i][0];
                //x_intercept = Math.floor(Math.random() * 2) == 1;
                if( x_intercept < 0 ) { x_intercept = 0; }
                if( x_intercept > size) { x_intercept = size; }
                for( y in 0...size) {
                    var new_color :Int;
                    new_color = (data.getPixel(x,y) & 0xff)+(y<x_intercept ? rise : drop);
                    if(new_color > 255) new_color = 255;
                    else if(new_color < 0) new_color = 0;
                    data.setPixel(x, y, (new_color << 16) + (new_color << 8) + new_color);
                }
            }
            current_i += 1;
            data.unlock();
        }
        current_o += 1;
    }
}
