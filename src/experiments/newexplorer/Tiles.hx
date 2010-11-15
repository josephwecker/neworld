package experiments.newexplorer;

import assets.creatures.Display;
import assets.creatures.Sounds;
import assets.terrain.Display;

import experiments.newexplorer.world.Column;
import experiments.newexplorer.ViewPort;
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

    var col_template     :flash.display.Bitmap;
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

    inline function real_col_height(in_height :Int) {
        //return Std.int(in_height / 0xffff * 512 / 9) * 9;
        return Std.int(in_height / 0xffff * 512 / 45) * 45;
    }

    public function render(col :Column, dat :BitmapData, x :Int, y :Int, ref_height :Int, ?hi=false) {
        var this_height  = real_col_height(col.total_height);
        var other_height = real_col_height(ref_height);
        var norm_hdiff = this_height - other_height;
        y = y - norm_hdiff;

        // Base color (will be replaced with shaders etc. at some point)
        var c = 2 * Math.floor(col.total_height / 0xffff * 64);
        var color = hi ? 0xFF000000+(c<<16)+((c>>1)<<8)+(c>>1) : 0xFF000000+((c>>1)<<16)+(c<<8)+(c>>1);

        var key = 'iso' + color;
        for(neighbor_num in 0...8) {
            var neighbor = col.n[D.rel(neighbor_num)];
            norm_hdiff = real_col_height(neighbor.total_height) - this_height;
            key += norm_hdiff;
        }

        var attribs :RenderOpts = {
            key:            key,
            base_color:     color,
            col_template:   col_template,
            width:          width,
            height:         height,
            top_renderer:   this.tile_render_top,
            col_renderer:   this.tile_render_col,
            column:         col
            };

        var bmd = render_pool.get_rendered(attribs);
        dat.copyPixels(bmd, bmd.rect, new flash.geom.Point(x,y - 9));
        if(hi) {
            dat.copyPixels(person.bitmapData, person.bitmapData.rect,
                new flash.geom.Point(x + ((width - person.width) / 2),
                                     y + (height / 2) - person.height +4));
        }
    }

    public function tile_render_top(opts:RenderOpts) : BitmapData {return null;}

    public function tile_render_col(opts:RenderOpts) : BitmapData {
        var dat = opts.col_template.bitmapData;
        var stamp = dat.getVector(dat.rect);
        
        //var i = 0;
        //for(pixel in stamp) {
        //    if(pixel== 4290145609 ) stamp[i] = opts.base_color;
        //    else if(pixel== 4285342720 ) stamp[i] = opts.base_color - 0x002200;
        //    else if(pixel== 4287777060 ) stamp[i] = opts.base_color + 0x002200;
        //    i += 1;
        //}
        
        var bmd = new BitmapData(dat.width, dat.height, true, 0);
        bmd.setVector(bmd.rect, stamp);
        return bmd;
    }
}

class DimetricTile extends TileRenderer {
    public function new() {
        col_template = new DimCol();
                     //  ax  ay  ox   w   h
        super(dimetric, 24, 12,  -2, 26, 12);
    }

    public override function tile_render_top(opts :RenderOpts) :BitmapData {
        var template = new flash.display.Shape();
        var center = [width/2, height/2+50];

        var diffs = new Array<Int>();
        for(neighbor_num in D.LEFT...(D.LEFT+8)) {
            var neighbor = opts.column.n[D.rel(neighbor_num)];
            var norm_hdiff = real_col_height(neighbor.total_height) -
                             real_col_height(opts.column.total_height);
            diffs[neighbor_num & 7] = norm_hdiff >> 1;
        }

        diffs[D.UPLEFT]    = (diffs[D.UPLEFT]    + diffs[D.LEFT]  + diffs[D.UP])   >>1;
        diffs[D.UPRIGHT]   = (diffs[D.UPRIGHT]   + diffs[D.RIGHT] + diffs[D.UP])   >>1;
        diffs[D.DOWNLEFT]  = (diffs[D.DOWNLEFT]  + diffs[D.LEFT]  + diffs[D.DOWN]) >>1;
        diffs[D.DOWNRIGHT] = (diffs[D.DOWNRIGHT] + diffs[D.RIGHT] + diffs[D.DOWN]) >>1;


        var vertices = new Array<Array<Float>>();
        vertices[D.LEFT]      = [ 1.0      , height/2 + 50 - (diffs[D.LEFT]      )];
        vertices[D.UPLEFT]    = [ 2.0      , 0.0      + 50 - (diffs[D.UPLEFT]    )];
        vertices[D.UP]        = [width/2+1 , 0.0      + 50 - (diffs[D.UP]        )];
        vertices[D.UPRIGHT]   = [width+0.0 , 0.0      + 50 - (diffs[D.UPRIGHT]   )];
        vertices[D.RIGHT]     = [width-1   , height/2 + 50 - (diffs[D.RIGHT]     )];
        vertices[D.DOWNRIGHT] = [width-2.0 , height   + 50 - (diffs[D.DOWNRIGHT] )];
        vertices[D.DOWN]      = [width/2-1 , height   + 50 - (diffs[D.DOWN]      )];
        vertices[D.DOWNLEFT]  = [ 0.0      , height   + 50 - (diffs[D.DOWNLEFT]  )];

        var bmd = new BitmapData(width,height+100,true,0x000000);

        for( poly in D.LEFT...(D.LEFT+8)) {
            template.graphics.beginFill(opts.base_color + poly *2);
            template.graphics.moveTo(center[0],center[1]);
            var node_1 = poly & 7;
            var node_2 = (poly + 1) & 7;
            template.graphics.lineTo(vertices[node_1][0], vertices[node_1][1]);
            template.graphics.lineTo(vertices[node_2][0], vertices[node_2][1]);
            template.graphics.endFill();
        }
        bmd.draw(template);
        return bmd;
    }
}

class IsometricTile extends TileRenderer {
    public function new() {
        col_template = new IsoCol();
                    //  ax  ay  ox   w   h
        super(isometric, 34, 9, 17, 34, 18);
    }

    public override function tile_render_top(opts :RenderOpts) :BitmapData {
        var template = new flash.display.Shape();
        var center = [width/2, height/2+50];

        var diffs = new Array<Int>();
        for(neighbor_num in D.LEFT...(D.LEFT+8)) {
            var neighbor = opts.column.n[D.rel(neighbor_num)];
            var norm_hdiff = real_col_height(neighbor.total_height) -
                             real_col_height(opts.column.total_height);
            diffs[neighbor_num & 7] = norm_hdiff>>1;
        }

        diffs[D.LEFT]  = (diffs[D.UPLEFT]   + diffs[D.LEFT]  + diffs[D.DOWNLEFT])>>1;
        diffs[D.UP]    = (diffs[D.UPLEFT]   + diffs[D.UP]    + diffs[D.UPRIGHT])>>1;
        diffs[D.RIGHT] = (diffs[D.UPRIGHT]  + diffs[D.RIGHT] + diffs[D.DOWNRIGHT])>>1;
        diffs[D.DOWN]  = (diffs[D.DOWNRIGHT]+ diffs[D.DOWN]  + diffs[D.DOWNLEFT])>>1;

        var shade_highlightest = opts.base_color - 0x003000;
        var shade_highlight = opts.base_color - 0x001500;
        var shade_flat = opts.base_color;
        var shade_shadow = opts.base_color + 0x001500;
        var shade_shadowest = opts.base_color + 0x003000;

        var vertices = new Array<Array<Float>>();
        vertices[D.LEFT]      = [ 0.0       , height/2   + 50 - (diffs[D.LEFT])];
        vertices[D.UPLEFT]    = [ width/4   , height/4   + 50 - (diffs[D.UPLEFT])];
        vertices[D.UP]        = [ width/2   , 0.0        + 50 - (diffs[D.UP])];
        vertices[D.UPRIGHT]   = [ width*3/4 , height/4   + 50 - (diffs[D.UPRIGHT])];
        vertices[D.RIGHT]     = [ width     , height/2   + 50 - (diffs[D.RIGHT])];
        vertices[D.DOWNRIGHT] = [ width*3/4 , height*3/4 + 50 - (diffs[D.DOWNRIGHT])];
        vertices[D.DOWN]      = [ width/2   , height     + 50 - (diffs[D.DOWN])];
        vertices[D.DOWNLEFT]  = [ width/4   , height*3/4 + 50 - (diffs[D.DOWNLEFT])];

        var bmd = new BitmapData(width,height+100,true,0x000000);

        // Fill the small triangles
        /*if(diffs[D.LEFT]) {
            template.graphics.beginFill(shade_shadow);
            template.graphics.moveTo(center[0],center[1]);
            template.graphics.lineTo(0.0, 8.5);
            template.graphics.lineTo(0.0, 17.5);
            template.graphics.lineTo(17.0, 26.0);
            template.graphics.endFill();
        }
        if(diffs[D.RIGHT]) {
            template.graphics.beginFill(shade_highlight);
            template.graphics.moveTo(center[0],center[1]);
            template.graphics.lineTo(34.0, 8.5);
            template.graphics.lineTo(34.0, 17.5);
            template.graphics.lineTo(17.0, 26.0);
            template.graphics.endFill();
        }

        if(diffs[D.DOWN]) {
            template.graphics.beginFill(shade_flat);
            template.graphics.moveTo(center[0],center[1]);
            template.graphics.lineTo(8.5, 21.75);
            template.graphics.lineTo(17.0, 26.0);
            template.graphics.lineTo(25.5, 21.7);
            template.graphics.endFill();
        }*/

        for( poly in D.LEFT...(D.LEFT+8)) {
            template.graphics.beginFill(opts.base_color+poly*2);
            template.graphics.moveTo(center[0],center[1]);
            var node_1 = poly & 7;
            var node_2 = (poly + 1) & 7;
            template.graphics.lineTo(vertices[node_1][0], vertices[node_1][1]);
            template.graphics.lineTo(vertices[node_2][0], vertices[node_2][1]);
            template.graphics.endFill();
        }
        bmd.draw(template);
        return bmd;
        //top_template = new flash.display.Bitmap(bmd);
    }
}

typedef RenderOpts = {
    var key            :String;
    var base_color     :UInt;
    var col_template   :flash.display.Bitmap;
    var width          :UInt;
    var height         :UInt;
    var top_renderer   :RenderOpts->BitmapData;
    var col_renderer   :RenderOpts->BitmapData;
    var column         :Column;
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
            var bmd = opts.top_renderer(opts);
            //var cbmd = opts.col_renderer(opts);
            //var bmd = new BitmapData(34,126,true,0x000000);
            //for( i in 18...bmd.height ) {
            //    bmd.copyPixels(cbmd, cbmd.rect, new flash.geom.Point(0,i));
            //}
            //bmd.copyPixels(tbmd, tbmd.rect, new flash.geom.Point(0,0),null,null,true);
            rendered.set(key, bmd);
            return bmd;
        }
    }

}
