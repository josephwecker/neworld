package experiments.newexplorer;

import experiments.newexplorer.world.Column;
import flash.display.BitmapData;
import flash.Vector;

enum TileType {dimetric; isometric;}

class Tile {
    public var type      :TileType;
    public var advance_x :Int;
    public var advance_y :Int;
    public var offset_x  :Int;
    public var width     :Int;
    public var height    :Int;
    // TODO: skew / drift for dimetric

    var top_template     :Array<Int>;
    var top_stamp        :Vector<UInt>;
    //var top_stamp        :Shape;
    public function new(type, ax, ay, ox, w, h) {
        this.type = type;
        advance_x = ax;
        advance_y = ay;
        offset_x  = ox;
        width     = w;
        height    = h;
    }

    public function render(col :Column, dat :BitmapData, x :Int, y :Int, ref_height :Int, ?hi=false) {
        if(top_template == null) return;
        var height_diff = col.total_height - ref_height;
        var norm_hdiff = Math.round(height_diff / 0xffff * 192);
        y = y - norm_hdiff;

        var base_color = 2 * Math.floor(col.total_height / 0xffff * 64);
        var attribs :RenderOpts = {
            key:             'iso' + base_color,
            base_color:      base_color,
            template:        top_template,
            template_width:  width,
            template_height: height,
            renderer:        this.tile_render
            };
        var bmd = renderpool.get_rendered(attribs);
        dat.copyPixels(bmd, bmd.rect, new flash.geom.Point(x,y));
    }

    public function tile_render(opts:RenderOpts) : BitmapData {
        var top_stamp = new Vector<UInt>(opts.width * opts.height, true);
        var i = 0;
        for(pixel in opts.template) {
            if(pixel == 0) top_stamp[i] = 0;
            else {
                var trans = 0xFF000000;
                var c = opts.base_color;
                if(hi) top_stamp[i] = trans + (c<<16)+((c>>1)<<8)+(c>>1); // redish
                else   top_stamp[i] = trans + ((c>>1)<<16)+(c<<8)+(c>>1); // greenish
            }
            i += 1;
        }

        var bmd = new BitmapData(opts.width, opts.height, true, 0);
        bmd.setVector(bmd.rect, top_stamp);
        //bmd.setVector(new flash.geom.Rectangle(0,0,opts.width,opts.height), top_stamp);
        return bmd;
    }
/*
            top_stamp = new Vector<UInt>(width * height, true);
            var i = 0;
            for(pixel in top_template) {
                if(pixel == 0) top_stamp[i] = 0;
                else {
                    var trans = 0xFF000000;
                    var c = pixel * Math.floor(col.total_height / 0xffff * 64);
                    if(hi) top_stamp[i] = trans + (c<<16)+((c>>1)<<8)+(c>>1); // redish
                    else   top_stamp[i] = trans + ((c>>1)<<16)+(c<<8)+(c>>1); // greenish
                }
                i += 1;
            }

            var bmd = new BitmapData(width, height, true, 0);
            bmd.setVector(new flash.geom.Rectangle(0,0,width,height), top_stamp);
            dat.copyPixels(bmd, bmd.rect, new flash.geom.Point(x,y));
        }
    }
*/
}

class DimetricTile extends Tile {
    public function new() {
                    //  ax  ay  ox   w   h
        super(dimetric, 24, 12,  4, 24, 12);
    }
}

class IsometricTile extends Tile {
    public function new() {
        top_template = [
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,2,2,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,2,2,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
            0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
            0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
            0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
            1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
            1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
            1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,
            0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
            0,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,0,0,0,
            0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,0,0,0,0,0,
            0,0,0,0,0,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,1,0,0,1,1,1,1,1,1,1,1,1,0,0,0,1,0,0,0,0,0,0,0,0];

            /*0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,3,3,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,3,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,3,3,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,3,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
            0,0,0,0,0,3,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
            0,0,0,3,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
            0,3,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
            3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
            1,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
            1,1,1,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
            0,0,1,1,1,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
            0,0,0,1,0,1,1,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
            0,0,0,0,0,1,1,1,1,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,1,0,1,0,1,1,2,2,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,1,1,1,2,2,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,1,0,0,1,1,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0];*/

                     //  ax  ay  ox   w   h
        super(isometric, 34, 9, 17, 34, 18);
    }
}

typedef RenderOpts = {
    var key             :String;
    var base_color      :UInt;
    var template        :Array<Int>;
    var width     :UInt;
    var height    :UInt;
    var renderer        :RenderOpts->BitmapData;
}

class RenderedTilePool {
    var rendered : Hash<BitmapData>;
    public function new() {
        rendered = new Hash<BitmapData>();
    }

    public function get_rendered(opts:RenderOpts) {
        var key = opts.key;
        if(rendered.exists(key)) return rendered.get(key);
        else {
            var bmd = opts.renderer(opts);
            rendered.set(key, bmd);
            return bmd;
        }
    }

}

/*class TileGraphic extends Shape {
    public function new() {
        super();
    }
}*/
