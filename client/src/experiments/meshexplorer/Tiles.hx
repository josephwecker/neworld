package experiments.meshexplorer;

import assets.creatures.Display;
import assets.creatures.Sounds;
import assets.terrain.Display;

import experiments.meshexplorer.world.Column;
import experiments.meshexplorer.ViewPort;
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

    var template         :flash.display.Shape;
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
        key += (col.cross);

        var attribs :RenderOpts = {
            key:            key,
            base_color:     color,
            width:          width,
            height:         height,
            top_renderer:   this.tile_render_top,
            column:         col
            };

        var bmd = render_pool.get_rendered(attribs);
        dat.copyPixels(bmd, bmd.rect, new flash.geom.Point(x,y - 9));
        if(hi) {
            dat.copyPixels(person.bitmapData, person.bitmapData.rect,
                new flash.geom.Point(x + ((width - person.width) / 2),
                                     y + (height / 2) - person.height +50));
        }
    }

    public function tile_render_top(opts:RenderOpts) : BitmapData {return null;}

    public function draw_rectangle( opts:RenderOpts, vertices:Array<Array<Int>> ) {
        template.graphics.lineStyle(0.25,0x008800);
        if( opts.column.cross ) {
            template.graphics.beginFill(opts.base_color);
            template.graphics.moveTo(vertices[0][0], vertices[0][1]);
            template.graphics.lineTo(vertices[1][0], vertices[1][1]);
            template.graphics.lineTo(vertices[2][0], vertices[2][1]);
            template.graphics.lineTo(vertices[0][0], vertices[0][1]);
            template.graphics.endFill();

            template.graphics.beginFill(opts.base_color);
            template.graphics.moveTo(vertices[2][0], vertices[2][1]);
            template.graphics.lineTo(vertices[1][0], vertices[1][1]);
            template.graphics.lineTo(vertices[3][0], vertices[3][1]);
            template.graphics.lineTo(vertices[2][0], vertices[2][1]);
            template.graphics.endFill();
        } else {
            template.graphics.beginFill(opts.base_color);
            template.graphics.moveTo(vertices[0][0], vertices[0][1]);
            template.graphics.lineTo(vertices[1][0], vertices[1][1]);
            template.graphics.lineTo(vertices[3][0], vertices[3][1]);
            template.graphics.lineTo(vertices[0][0], vertices[0][1]);
            template.graphics.endFill();

            template.graphics.beginFill(opts.base_color);
            template.graphics.moveTo(vertices[0][0], vertices[0][1]);
            template.graphics.lineTo(vertices[2][0], vertices[2][1]);
            template.graphics.lineTo(vertices[3][0], vertices[3][1]);
            template.graphics.lineTo(vertices[0][0], vertices[0][1]);
            template.graphics.endFill();
        }
    }
}

class DimetricTile extends TileRenderer {
    public function new() {
                    //  ax  ay  ox   w   h
        super(dimetric, 24, 12,  0, 24, 12);
     }

    public override function tile_render_top(opts :RenderOpts) :BitmapData {
        template = new flash.display.Shape();

        var verts = new Array<Int>();
        for(neighbor_num in D.RIGHT...(D.RIGHT+3)) {
            var neighbor = opts.column.n[D.rel(neighbor_num)];
            var norm_hdiff = real_col_height(neighbor.total_height) -
                             real_col_height(opts.column.total_height);
            verts.push(norm_hdiff);
        }

        var vertices = new Array<Array<Int>>();
        vertices[0] = [ 0     , 0      + 50           ];
        vertices[1] = [ width , 0      + 50 ];//- verts[0] ];
        vertices[2] = [ 0     , height + 50 ];//- verts[1] ];
        vertices[3] = [ width , height + 50 ];//- verts[2] ];

        var bmd = new BitmapData(width,height+100,true,0x000000);
        
        this.draw_rectangle(opts, vertices);
        
        bmd.draw(template);
        return bmd;
    }
}

class IsometricTile extends TileRenderer {
    public function new() {
                     //  ax  ay  ox   w   h
        super(isometric, 34, 9, 17, 34, 18);
    }

    public override function tile_render_top(opts :RenderOpts) :BitmapData {
        template = new flash.display.Shape();

        var verts = new Array<Int>();
        for(neighbor_num in D.DOWNRIGHT...(D.DOWNRIGHT+3)) {
            var neighbor = opts.column.n[D.rel(neighbor_num)];
            var norm_hdiff = real_col_height(neighbor.total_height) -
                             real_col_height(opts.column.total_height);
            verts.push(norm_hdiff);
        }

        var vertices = new Array<Array<Int>>();
        vertices[0] = [ 17 , 0 + 50 ];
        vertices[1] = [ 34 , 9 + 50 ];//- verts[0] ];
        vertices[2] = [ 0  , 9 + 50 ];//- verts[2] ];
        vertices[3] = [ 17 , 18    + 50 ];//- verts[1] ];
        //vertices[0] = [ width>>2 , 0         + 50            ];
        //vertices[1] = [ width    , height>>2 + 50 ];//- verts[0] ];
        //vertices[2] = [ width>>2 , height    + 50 ];//- verts[1] ];
        //vertices[3] = [ 0        , height>>2 + 50 ];//- verts[2] ];

        var bmd = new BitmapData(width,height+100,true,0x000000);
        
        this.draw_rectangle(opts, vertices);
        
        bmd.draw(template);
        return bmd;
    }
}

typedef RenderOpts = {
    var key            :String;
    var base_color     :UInt;
    var width          :UInt;
    var height         :UInt;
    var top_renderer   :RenderOpts->BitmapData;
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
            rendered.set(key, bmd);
            return bmd;
        }
    }
}
