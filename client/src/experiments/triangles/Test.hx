package experiments.triangles;
import flash.events.KeyboardEvent;
using neworld.Utils;

class Test {
    var map :DrawMap;

    public static function main() {
        flash.Lib.current.cacheAsBitmap =               true;
        flash.Lib.current.stage.scaleMode =             flash.display.StageScaleMode.NO_SCALE;
        flash.Lib.current.stage.align =                 flash.display.StageAlign.TOP_LEFT;
        flash.Lib.current.stage.showDefaultContextMenu= false;
        flash.Lib.current.contextMenu =                 new flash.ui.ContextMenu();
        flash.Lib.current.mouseEnabled =                false;
        flash.Lib.current.contextMenu.hideBuiltInItems();
        var triangles   = new Test();
    }

    public function new() {
        map = new DrawMap();
        flash.Lib.current.stage.addEventListener(KeyboardEvent.KEY_DOWN, rotate);
    }
    
    function rotate(event:KeyboardEvent) {
        map.rotate();
    }
}

class DrawMap {
    var hm      :flash.Vector<UInt>;
    var picture :flash.display.Shape;
    var vertices    :flash.Vector<Float>;
    var indices     :flash.Vector<Int>;

    public function new() {

        vertices        = new flash.Vector<Float>();
        indices         = new flash.Vector<Int>();
        
        var hm_array :Array<UInt> = 
             [00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 
              00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 
              00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 
              00, 00, 00, 00, 00, 00, 40, 00, 00, 00, 00, 00, 00, 00, 00, 
              00, 00, 00, 00, 00, 00, 00, 40, 40, 00, 00, 30, 00, 00, 00, 
              00, 00, 00, 00, 00, 00, 40, 60, 40, 00, 00, 00, 00, 00, 00, 
              20, 20, 20, 20, 20, 50, 60, 70, 60, 00, 00, 00, 00, 00, 40, 
              00, 00, 00, 00, 00, 60, 80, 60, 40, 00, 00, 00, 00, 40, 00, 
              00, 00, 00, 00, 00, 00, 60, 50, 00, 40, 00, 00, 00, 40, 00, 
              00, 00, 00, 00, 00, 00, 00, 50, 00, 00, 40, 40, 40, 00, 00, 
              00, 00, 00, 00, 00, 00, 00, 50, 00, 00, 00, 00, 00, 00, 00, 
              00, 00, 00, 00, 00, 00, 00, 50, 00, 00, 00, 00, 00, 00, 00, 
              00, 00, 00, 00, 00, 00, 00, 50, 00, 00, 00, 00, 00, 00, 00, 
              00, 00, 00, 00, 00, 00, 00, 40, 00, 00, 00, 00, 00, 00, 00, 
              00, 00, 00, 00, 00, 00, 00, 40, 00, 00, 00, 00, 00, 00, 00];
        hm = hm_array.toUIntVector();
            var bmd    = new flash.display.BitmapData(15,15,false,0);
            var myrect = new flash.geom.Rectangle(0,0,15,15);
            bmd.setVector(myrect,hm);
            bmd.draw(bmd);
        var bmp = new flash.display.Bitmap(bmd);
        flash.Lib.current.addChild(bmp);
            hm = bmd.getVector(myrect);
        

        this.update_cardinal_vertices();
        this.update_cardinal_indices();
        this.draw();
        flash.Lib.current.addChild(picture);
    }

    public function update_vertices() {
        var vertices_array  = new Array<Float>();
        var width = 48;
        var height = 24;
        var col_count = 15;
        var row_count = 15;
        for( count in 0...(row_count*col_count) ) {
            vertices_array.push(count%col_count*width+50);
            vertices_array.push(Math.floor(count/col_count)*height-(hm[count]-4278190080)+50);
        }
        vertices = vertices_array.toFloatVector();
    }

    public function update_cardinal_vertices() {
        var vertices_array  = new Array<Float>();
        var width = 68;
        var height = 17;
        var offset = 34;
        var col_count = 15;
        var row_count = 15;
        for( count in 0...(row_count*col_count) ) {
            if( Math.floor(count/col_count)%2 == 0 ) {
                vertices_array.push(count%col_count*width+50);
            } else {
                vertices_array.push(count%col_count*width+offset+50);
            }
            vertices_array.push(Math.floor(count/col_count)*height-(hm[count]-4278190080)+50);
        }
        vertices = vertices_array.toFloatVector();
    }

    public function update_indices() {
        var indices_array   = new Array<Int>();
        var col_count = 15;
        var row_count = 15;
        var row_tick = true;
        var col_tick = true;
        for( row in 0...(row_count-1) ) {
            col_tick = true;
            for( col in 0...(col_count-1)) {
                if( row_tick == col_tick ) {
                    indices_array.push(row*col_count+col);
                    indices_array.push(row*col_count+col+1);
                    indices_array.push((row+1)*col_count+col+1);
                    indices_array.push(row*col_count+col);
                    indices_array.push((row+1)*col_count+col);
                    indices_array.push((row+1)*col_count+col+1);
                } else {
                    indices_array.push(row*col_count+col+1);
                    indices_array.push(row*col_count+col);
                    indices_array.push((row+1)*col_count+col);
                    indices_array.push(row*col_count+col+1);
                    indices_array.push((row+1)*col_count+col+1);
                    indices_array.push((row+1)*col_count+col);
                }
                col_tick = !col_tick;
            }
            row_tick = !row_tick;
        }
        indices  = indices_array.toIntVector();
    }

    public function update_cardinal_indices() {
        var indices_array   = new Array<Int>();
        var col_count = 15;
        var row_count = 15;
        var row_tick = false;
        for( row in 0...(row_count-2) ) {
            for( col in 0...(col_count-1)) {
                if( row_tick ) {
                    indices_array.push(row*col_count+col);
                    indices_array.push((row+1)*col_count+col);
                    indices_array.push((row+2)*col_count+col);
                    indices_array.push(row*col_count+col);
                    indices_array.push((row+1)*col_count+col+1);
                    indices_array.push((row+2)*col_count+col);
                } else {
                    indices_array.push(row*col_count+col+1);
                    indices_array.push((row+1)*col_count+col+1);
                    indices_array.push((row+1)*col_count+col);
                    indices_array.push((row+1)*col_count+col);
                    indices_array.push((row+1)*col_count+col+1);
                    indices_array.push((row+2)*col_count+col+1);
                }
            }
            row_tick = !row_tick;
        }
        indices  = indices_array.toIntVector();
    }

    public function draw() {
        picture = new flash.display.Shape();
        picture.graphics.lineStyle(0.2,0x000088);
        for( tri in 0...(Math.floor(indices.length/3)) ) {
            var i = tri*3;
            picture.graphics.beginFill(tri>>1);
            picture.graphics.moveTo(vertices[indices[i+0]*2],vertices[indices[i+0]*2+1]);
            picture.graphics.lineTo(vertices[indices[i+1]*2],vertices[indices[i+1]*2+1]);
            picture.graphics.lineTo(vertices[indices[i+2]*2],vertices[indices[i+2]*2+1]);
            picture.graphics.lineTo(vertices[indices[i+0]*2],vertices[indices[i+0]*2+1]);
            picture.graphics.endFill();
        }
    }

    public function rotate() {
        var bmd    = new flash.display.BitmapData(15,15,false,10);
        var bmd2   = new flash.display.BitmapData(15,15,false,10);
        var my_rectangle = new flash.geom.Rectangle(0,0,15,15);
        var my_matrix = new flash.geom.Matrix();
        var old_hm = hm;
        hm = new flash.Vector<UInt>();
        bmd.setVector(my_rectangle,old_hm);
        my_matrix.rotate(Math.PI/2);
        my_matrix.translate(15,0);
        bmd2.draw(bmd,my_matrix);
        var bmp = new flash.display.Bitmap(bmd2);
        flash.Lib.current.addChild(bmp);
        hm = bmd2.getVector(my_rectangle);
        this.update_cardinal_vertices();
        this.update_cardinal_indices();
        flash.Lib.current.removeChild(picture);
        this.draw();
        flash.Lib.current.addChild(picture);
    }
    
}

