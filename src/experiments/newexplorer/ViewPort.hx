package experiments.newexplorer;

import experiments.newexplorer.world.World;
import experiments.newexplorer.world.Column;
import flash.display.BitmapData;
import flash.display.Bitmap;

class D {
    static var _instance = new D();
    var _orientation :Int;
    public static var orientation(get_orientation, set_orientation) :Int;
    public static var tile_type(get_tile_type, null) :TileType;

    public inline static var NORTH     = 0;
    public inline static var NORTHEAST = 1;
    public inline static var EAST      = 2;
    public inline static var SOUTHEAST = 3;
    public inline static var SOUTH     = 4;
    public inline static var SOUTHWEST = 5;
    public inline static var WEST      = 6;
    public inline static var NORTHWEST = 7;

    public inline static var UP        = 0;
    public inline static var UPRIGHT   = 1;
    public inline static var RIGHT     = 2;
    public inline static var DOWNRIGHT = 3;
    public inline static var DOWN      = 4;
    public inline static var DOWNLEFT  = 5;
    public inline static var LEFT      = 6;
    public inline static var UPLEFT    = 7;

    public static function rel(dir:Int) {return (dir + _instance._orientation) % 8;}

    private function new() {
        _orientation = NORTH;
    }

    static function get_tile_type() :TileType {
        if(_instance._orientation & 1 == 1) return isometric;
        else return dimetric;
    }

    static function get_orientation() :Int {return _instance._orientation;}
    static function set_orientation(o:Int) :Int {
        _instance._orientation = o;
        return o;
    }
}

enum TileType {dimetric; isometric;}

class Tile {
    public var type:       TileType;
    public var advance_x : Int;
    public var advance_y : Int;
    public var offset_x :  Int;
    public var width:      Int;
    public var height:     Int;
    public function new(type, ax, ay, ox, w, h) {
        this.type = type;
        advance_x = ax;
        advance_y = ay;
        offset_x  = ox;
        width     = w;
        height    = h;
    }

    public function render(col :Column, dat :BitmapData, x :Int, y :Int, ref_height :Int) {
        dat.setPixel(x + (width >> 1), y + (height >> 1), 0x990000);
        /*trace([
                'col height: '+col.total_height,
                'x:          '+x,
                'y:          '+y,
                'ref_height: '+ref_height
                ].join("\n"));*/
    }
}

class DimetricTile extends Tile {
    public function new() {
                    //  ax  ay  ox   w   h
        super(dimetric, 24, 12,  4, 24, 12);
    }
}

class IsometricTile extends Tile {
    public function new() {
                     //  ax  ay  ox   w   h
        super(isometric, 34, 10, 17, 34, 17);
    }
}


class ViewPort extends flash.display.Sprite { // For events
    var portal_width  (get_portal_width, null)  : Int;
    var portal_height (get_portal_height, null) : Int;
    var view_data   : BitmapData;
    var main_bitmap : Bitmap;
    var world       : World;
    var isotile     : Tile;
    var dimtile     : Tile;

    public function new(world : World) {
        super();
        this.world = world;
        isotile = new IsometricTile();
        dimtile = new DimetricTile();

        flash.Lib.current.addChild(this);
        flash.Lib.current.cacheAsBitmap =   true;
        stage.scaleMode =                   flash.display.StageScaleMode.NO_SCALE;
        stage.align =                       flash.display.StageAlign.TOP_LEFT;
        stage.showDefaultContextMenu=       false;
        flash.Lib.current.contextMenu =     new flash.ui.ContextMenu();
        flash.Lib.current.contextMenu.hideBuiltInItems();
        flash.Lib.current.mouseEnabled =    false;

        main_bitmap = new Bitmap(null, flash.display.PixelSnapping.ALWAYS, false);
        calculate_boundaries(null);
        flash.Lib.current.addChild(main_bitmap);

        stage.addEventListener(flash.events.Event.RESIZE, calculate_boundaries);
        stage.addEventListener(flash.events.KeyboardEvent.KEY_DOWN, maybe_fullscreen);
    }

    inline function get_portal_width() : Int { return stage.stageWidth;}
    inline function get_portal_height(): Int { return stage.stageHeight;}

    function calculate_boundaries(_) {
        view_data = new BitmapData(portal_width, portal_height, false, 0xFFFFFF);
        main_bitmap.bitmapData = view_data;
        stage.scaleMode = flash.display.StageScaleMode.NO_SCALE;
        stage.align = flash.display.StageAlign.TOP_LEFT;
        main_bitmap.width = portal_width;
        main_bitmap.height = portal_height;
    }

    function maybe_fullscreen(e : flash.events.KeyboardEvent) {
        try {
            if(e.keyCode == 70) {
                trace("Toggling fullscreen");
                if(stage.displayState == flash.display.StageDisplayState.NORMAL)
                    stage.displayState = flash.display.StageDisplayState.FULL_SCREEN;
                else stage.displayState = flash.display.StageDisplayState.NORMAL;
            }
        } catch(_:Dynamic) {}
    }

    public function render_from(mid_col:Column, top_dir:Int, ?long_offset=0, ?lat_offset=0) {
        view_data.lock();

        D.orientation = top_dir;
        var tile = D.tile_type == isometric ? isotile : dimtile;
        var center_height = mid_col.total_height;

        // Find top-left corner - start_vpx, start_vpy & column
        var vp_center_x :Int = portal_width  >> 1;
        var vp_center_y :Int = portal_height >> 1;
        var up_tiles    :Int = Math.floor(vp_center_y / tile.height) + 2;
        var over_tiles  :Int = Math.floor(vp_center_x / tile.width) + 2;
        // TODO: long/lat offset added to these offsets, depending on orientation
        var vx          :Int = vp_center_x - (over_tiles * tile.width) - (tile.width >> 1);
        var vy          :Int = vp_center_y - (up_tiles * tile.height) - (tile.height >> 1);

        var vx_left_boundary = vx;

        var curr_col = mid_col.move_by(D.UP, up_tiles);
        curr_col = curr_col.move_by(D.LEFT, over_tiles);
        while(vy < portal_height) {
            var leftmost_col = curr_col;
            var leftmost_vx  = vx;
            while(vx < portal_width) {
                // TODO: if center_column send flag in for special rendering
                tile.render(curr_col, view_data, vx, vy, center_height);
                vx += tile.advance_x;
                curr_col = curr_col.n[D.rel(D.RIGHT)];
            }
            // Reset to far left column then down-left one
            curr_col = leftmost_col.n[D.rel(D.DOWNLEFT)];
            vx = leftmost_vx - tile.advance_x + tile.offset_x;
            vy += tile.advance_y;

            if(vx < vx_left_boundary) {
                curr_col = curr_col.n[D.rel(D.RIGHT)];
                vx += tile.advance_x;
            }
        }

        trace([ 'tile:        ' + tile.type,
                'vp_center_x: ' + vp_center_x,
                'vp_center_y: ' + vp_center_y,
                'up_tiles:    ' + up_tiles,
                'over_tiles:  ' + over_tiles,
                'vx:          ' + vx,
                'vy:          ' + vy,
                ].join("\n"));

        view_data.setPixel(vp_center_x, vp_center_y, 0x000000);
        view_data.setPixel(vp_center_x+1, vp_center_y + 1, 0x00ff00);
        view_data.setPixel(vp_center_x+1, vp_center_y - 1, 0x00ff00);
        view_data.setPixel(vp_center_x-1, vp_center_y + 1, 0x00ff00);
        view_data.setPixel(vp_center_x-1, vp_center_y - 1, 0x00ff00);
        view_data.unlock();
    }
}


