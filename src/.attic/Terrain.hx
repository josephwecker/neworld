class Terrain {
    // 0: air
    // 1: granite
    // 2: sand
    // 3: water
    static var voxels = [
     [[0, 0, 0, 0, 3, 0, 0, 0, 0],
      [0, 0, 2, 2, 3, 2, 2, 0, 0],
      [0, 2, 2, 3, 0, 3, 2, 2, 0],
      [0, 2, 3, 0, 0, 0, 3, 2, 0],
      [0, 3, 0, 0, 0, 0, 0, 3, 0],
      [0, 0, 1, 0, 0, 0, 1, 0, 0],
      [0, 0, 0, 1, 0, 1, 0, 0, 0],
      [0, 0, 0, 3, 1, 3, 0, 0, 0]]
      ];

    var shape : flash.display.Shape;
    var g : flash.display.Graphics;

    static var cols = new Hash<TerrainColumn>();

    static var terrains = [
        new Air(),
        new Granite(),
        new Sand(),
        new Water()];

    public function new() {
        shape = new flash.display.Shape();
        g = shape.graphics;
        var mc = flash.Lib.current;
        mc.addChild(shape);
        var ymax = voxels[0].length - 1;
        var xmax = voxels[0][0].length - 1;
        for(y in 0...ymax+1) {
            for(x in 0...xmax+1) {
                var new_col = new TerrainColumn();
                for(z in 0...voxels.length) new_col.push_segment(terrains[voxels[z][y][x]], 1);
                cols.set(ckey(x,y), new_col);
            }
        }
        for(y in 0...ymax+1) {
            for(x in 0...xmax+1) {
                var new_col = cols.get(ckey(x,y));
                var right = (x == xmax) ? 0 : x + 1;
                var left  = (x == 0) ? xmax : x - 1;
                var down  = (y == ymax) ? 0 : y + 1;
                var up    = (y == 0) ? ymax : y - 1;
                new_col.next_000 = cols.get(ckey(x,     up  ));
                new_col.next_045 = cols.get(ckey(right, up  ));
                new_col.next_090 = cols.get(ckey(right, y   ));
                new_col.next_135 = cols.get(ckey(right, down));
                new_col.next_180 = cols.get(ckey(x,     down));
                new_col.next_225 = cols.get(ckey(left,  down));
                new_col.next_270 = cols.get(ckey(left,  y   ));
                new_col.next_315 = cols.get(ckey(left,  up  ));
            }
        }

        render(4,4);
    }

    static inline function ckey(x,y) {return Std.int(x) + '|' + Std.int(y);}

    static var TILE_VIEW_W = 30;
    static var TILE_VIEW_H = 60;

    static var CTILE_W = 40.0;
    static var CTILE_H = CTILE_W / 2.0;
    static var DTILE_W = Math.sqrt(2 * CTILE_W * CTILE_W);
    static var DTILE_H = DTILE_W / 2.0;

    public function render(?tile_center_x=0, ?tile_center_y=0) {
        g.clear();
        for(y in 0...voxels[0].length) {
            for(x in 0...voxels[0][0].length) {
                cols.get(ckey(x,y)).origin = false;
            }
        }
        // Traverse to upper-left
        // With 045 degrees added for 045 orientation
        var tile_curr = cols.get(ckey(tile_center_x, tile_center_y));
        tile_curr.origin = true;
        // Diagonal up-left
        for(i in 0...TILE_VIEW_W) tile_curr = tile_curr.next_000;
        // Left more or up more if needed
        if(TILE_VIEW_W > TILE_VIEW_H) {
            for(i in 0...((TILE_VIEW_W>>1)-(TILE_VIEW_H>>1))) tile_curr = tile_curr.next_315;
        } else if(TILE_VIEW_H > TILE_VIEW_W) {
            for(i in 0...((TILE_VIEW_H>>1)-(TILE_VIEW_W>>1))) tile_curr = tile_curr.next_045;
        }

        var curr_y = 0.0;
        for(i in 0...((TILE_VIEW_H>>1)+1)) {
            var curr_x = -(DTILE_W / 2.0);
            for(j in 0...TILE_VIEW_W) {
                draw_col(tile_curr, curr_x, curr_y);
                curr_x += DTILE_W;
                tile_curr = tile_curr.next_135;   // Right
            }
            curr_x -= DTILE_W + (DTILE_W / 2.0);
            curr_y += DTILE_H / 2.0;
            tile_curr = tile_curr.next_315; // Left
            tile_curr = tile_curr.next_270; // Down-left
            for(j in 0...(TILE_VIEW_W-1)) {
                draw_col(tile_curr, curr_x, curr_y);
                curr_x -= DTILE_W;
                tile_curr = tile_curr.next_315;   // Left
            }
            curr_y += DTILE_H / 2.0;
            tile_curr = tile_curr.next_180; // Down-right
        }

    }

    function draw_col(col : TerrainColumn, x, y) {
        draw_diamond(x, y, col.top_type().base_color);
    }

    function draw_diamond(x, y, color) {
        g.lineStyle(1, color | 0x111111);
        g.beginFill(color);
        g.moveTo(x,y);
        g.lineTo(x+(DTILE_W / 2.0), y-(DTILE_H / 2.0));
        g.lineTo(x+DTILE_W, y);
        g.lineTo(x+(DTILE_W / 2.0), y+(DTILE_H / 2.0));
        g.lineTo(x, y);
        g.endFill();
    }
}

class TerrainColumn {
    public var total_height : Int;
    public var head_segment : TerrainSegment;
    public var origin : Bool;

    public var next_000 : TerrainColumn;  // North      | Up
    public var next_045 : TerrainColumn;  // NorthEast  | Up   Right
    public var next_090 : TerrainColumn;  // East       |      Right
    public var next_135 : TerrainColumn;  // SouthEast  | Down Right

    public var next_180 : TerrainColumn;  // South      | Down
    public var next_225 : TerrainColumn;  // SouthWest  | Down Left
    public var next_270 : TerrainColumn;  // West       |      Left
    public var next_315 : TerrainColumn;  // NorthWest  | Up   Left

    public function new() {
        head_segment = null;
    }

    public function push_segment(type, height) {
        if(head_segment != null) {
            if(head_segment.type == type) {
                head_segment.height += height;
                return;
            }
        }
        var new_seg = new TerrainSegment(head_segment);
        head_segment = new_seg;
        head_segment.type = type;
        head_segment.height = height;
    }

    public function top_type() : TerrainType {
        if(origin) {
            return new TerrainType('lava', liquid, 0xFF0000);
        }
        if(head_segment == null) return new Air();
        return head_segment.type;
    }
}

class TerrainSegment {
    public var height : Int;
    public var next : TerrainSegment;
    public var type : TerrainType;

    public function new(next) { this.next = next; }
}


// How to simulate collapses?

enum TerrainTypeState {solid; liquid; gas;}
class TerrainType {
    public var name : String;
    public var base_color : UInt;
    public var state : TerrainTypeState;
    public var absorption_rate : Float;  // Better as a linkage between classes
    public var roughness : Float;        // Ditto... (as friction)
    public function new(name, state, ?base_color=0xEEEEEE) {
        this.state = state;
        this.base_color = base_color;
        this.name = name;
    }
}
private class Air extends TerrainType {
    public function new() {super('air',gas);}
}
private class Water extends TerrainType {
    public function new() {super('water',liquid, 0x0000FF);}
}
private class Sand extends TerrainType {
    public function new() {super('sand',solid, 0xFFCC99);}
}
private class Granite extends TerrainType {
    public function new() {super('granite',solid, 0x838B8B);}
}

