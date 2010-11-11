package experiments.newexplorer;

import assets.creatures.Display;
import assets.creatures.Sounds;
import assets.terrain.Display;

import experiments.newexplorer.world.Column;
import flash.display.BitmapData;
import flash.Vector;

import haxe.macro.Expr;
import haxe.macro.Context;

enum TileType {dimetric; isometric;}

class TileRenderer {
    public var type      :TileType;
    public var advance_x :Int;
    public var advance_y :Int;
    public var offset_x  :Int;
    public var width     :Int;
    public var height    :Int;
    // TODO: skew / drift for dimetric

    var top_template     :flash.display.Bitmap;
    var mid_template     :flash.display.Bitmap;
    var rep_template     :flash.display.Bitmap;
    var top_stamp        :Vector<UInt>;
    var mid_stamp        :Vector<UInt>;
    var rep_stamp        :Vector<UInt>;
    var render_pool      :RenderedTilePool;
    var person           :HumanoidSm;

    public function new(type, ax, ay, ox, w, h) {
        this.type = type;
        advance_x = ax;
        advance_y = ay;
        offset_x  = ox;
        width     = w;
        height    = h;
        render_pool = new RenderedTilePool();
        person = new HumanoidSm();
    }

    public function render(col :Column, dat :BitmapData, x :Int, y :Int, ref_height :Int, ?hi=false) {
        if(top_template == null) return;
        //var this_height  = (Std.int(col.total_height / 0xffff * 512) >> 3) << 3;
        //var other_height = (Std.int(ref_height / 0xffff * 512) >> 3) << 3;
        var this_height  = Std.int(col.total_height / 0xffff * 512 / 9) * 9;
        var other_height = Std.int(ref_height / 0xffff * 512 / 9) * 9;
        var norm_hdiff = this_height - other_height;
        y = y - norm_hdiff;

        var base_color = 2 * Math.floor(col.total_height / 0xffff * 64);
        if(hi) {
            base_color = 0xFF000000 +
                (base_color<<16)+((base_color>>1)<<8)+(base_color>>1);
        } else {
            base_color = 0xFF000000 +
                ((base_color>>1)<<16)+(base_color<<8)+(base_color>>1);
        }
        var attribs :RenderOpts = {
            key:        'iso' + base_color,
            base_color: base_color,
            top_template:   top_template,
            mid_template:   mid_template,
            rep_template:   rep_template,
            width:      width,
            height:     height,
            top_renderer:   this.tile_render_top,
            mid_renderer:   this.tile_render_mid,
            rep_renderer:   this.tile_render_rep};

        var bmd = render_pool.get_rendered(attribs);
        dat.copyPixels(bmd, bmd.rect, new flash.geom.Point(x,y));
        if(hi) {
            //var p = cast(person, Bitmap);
            dat.copyPixels(person.bitmapData, person.bitmapData.rect,
                new flash.geom.Point(x + ((width - person.width) / 2),
                                     y + (height / 2) - person.height +4));
        }
    }

    public function tile_render_top(opts:RenderOpts) : BitmapData {
        var dat = opts.top_template.bitmapData;
        var stamp = dat.getVector(dat.rect);
        
        //var top_stamp = new Vector<UInt>(opts.width * opts.height, true);
        var i = 0;
        for(pixel in stamp) {
            if(pixel == 0) stamp[i] = 0;
            else if(pixel==4278218020) stamp[i] = opts.base_color;
            i += 1;
        }
        
        var bmd = new BitmapData(dat.width, dat.height, true, 0);
        bmd.setVector(bmd.rect, stamp);
        return bmd;
    }

    public function tile_render_mid(opts:RenderOpts) : BitmapData {
        var dat = opts.mid_template.bitmapData;
        var stamp = dat.getVector(dat.rect);
        
        //var top_stamp = new Vector<UInt>(opts.width * opts.height, true);
        var i = 0;
        for(pixel in stamp) {
            if(pixel == 0) stamp[i] = 0;
            else if(pixel==230) stamp[i] = opts.base_color;
            i += 1;
        }
        
        var bmd = new BitmapData(dat.width, dat.height, true, 0);
        bmd.setVector(bmd.rect, stamp);
        return bmd;
    }

    public function tile_render_rep(opts:RenderOpts) : BitmapData {
        var dat = opts.rep_template.bitmapData;
        var stamp = dat.getVector(dat.rect);
        
        //var top_stamp = new Vector<UInt>(opts.width * opts.height, true);
        var i = 0;
        for(pixel in stamp) {
            if(pixel == 0) stamp[i] = 0;
            else if(pixel==0x006D24) stamp[i] = opts.base_color;
            i += 1;
        }
        
        var bmd = new BitmapData(dat.width, dat.height, true, 0);
        bmd.setVector(bmd.rect, stamp);
        return bmd;
    }
}

class DimetricTile extends TileRenderer {
    public function new() {
        top_template = new DimTop();
        mid_template = new DimMid();
        rep_template = new DimRep();
                     //  ax  ay  ox   w   h
        super(dimetric, 24, 12,  4, 24, 12);
    }
}

class IsometricTile extends TileRenderer {
    public function new() {
        top_template = new IsoTop();
        mid_template = new IsoMid();
        rep_template = new IsoRep();
                    //  ax  ay  ox   w   h
        super(isometric, 34, 9, 17, 34, 18);
    }
}

typedef RenderOpts = {
    var key        :String;
    var base_color :UInt;
    var top_template   :flash.display.Bitmap;
    var mid_template   :flash.display.Bitmap;
    var rep_template   :flash.display.Bitmap;
    var width      :UInt;
    var height     :UInt;
    var top_renderer   :RenderOpts->BitmapData;
    var mid_renderer   :RenderOpts->BitmapData;
    var rep_renderer   :RenderOpts->BitmapData;
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
            var tbmd = opts.top_renderer(opts);
            var mbmd = opts.mid_renderer(opts);
            var rbmd = opts.rep_renderer(opts);
            var bmd = new BitmapData(34,100,true,0x000000);
            for( i in 17...bmd.height ) {
                bmd.copyPixels(rbmd, rbmd.rect, new flash.geom.Point(0,i));
            }
            bmd.copyPixels(mbmd, mbmd.rect, new flash.geom.Point(0,0));
            bmd.copyPixels(tbmd, tbmd.rect, new flash.geom.Point(0,0),null,null,true);
            rendered.set(key, bmd);
            return bmd;
        }
    }

}
