package experiments.triangles;
using neworld.Utils;

class Test {

    public static function main() {
        flash.Lib.current.cacheAsBitmap =               true;
        flash.Lib.current.stage.scaleMode =             flash.display.StageScaleMode.NO_SCALE;
        flash.Lib.current.stage.align =                 flash.display.StageAlign.TOP_LEFT;
        flash.Lib.current.stage.showDefaultContextMenu= false;
        flash.Lib.current.contextMenu =                 new flash.ui.ContextMenu();
        flash.Lib.current.mouseEnabled =                false;
        flash.Lib.current.contextMenu.hideBuiltInItems();

        var triangles = new Test();
    }

    public function new() {
        var vertices        = new flash.Vector<Float>();
        var indices         = new flash.Vector<Int>();
        var hm              = new flash.Vector<UInt>();
        var vertices_array  = new Array<Float>();
        var indices_array   = new Array<Int>();
        var picture         = new flash.display.Shape();


        var hm_array :Array<UInt> = [00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 
                  00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 
                  00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 
                  00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 
                  00, 00, 00, 00, 00, 00, 00, 50, 00, 00, 00, 00, 00, 00, 00, 
                  00, 00, 00, 00, 00, 00, 50, 60, 50, 00, 00, 00, 00, 00, 00, 
                  00, 20, 20, 20, 20, 50, 60, 70, 60, 50, 00, 00, 00, 00, 00, 
                  00, 00, 00, 00, 00, 00, 50, 60, 50, 30, 00, 00, 00, 00, 00, 
                  00, 00, 00, 00, 00, 00, 00, 50, 00, 00, 30, 00, 00, 00, 00, 
                  00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 20, 20, 20, 20, 
                  00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 
                  00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 
                  00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 
                  00, 20, 00, 00, 00, 00, 40, 40, 00, 00, 00, 00, 00, 00, 00, 
                  20, 00, 00, 00, 00, 00, 40, 40, 00, 00, 00, 00, 00, 00,
                  00];
        hm = hm_array.toUIntVector();
        var bmd = new flash.display.BitmapData(15,15);
        bmd.setVector(bmd.rect,hm);
       

        var width = 24;
        var height = 12;
        var col_count = 15;
        var row_count = 15;
        for( count in 0...((row_count)*(col_count)) ) {
            vertices_array.push(count%(col_count)*width+50);
            vertices_array.push(Math.floor(count/(col_count))*height-hm[count]+50);
        }

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
        
        
        vertices = vertices_array.toFloatVector();
        indices  = indices_array.toIntVector();

        for( tri in 0...(Math.floor(indices.length/3)) ) {
            var i = tri*3;
            picture.graphics.beginFill(tri>>1);
            picture.graphics.moveTo(vertices[indices[i+0]*2],vertices[indices[i+0]*2+1]);
            picture.graphics.lineTo(vertices[indices[i+1]*2],vertices[indices[i+1]*2+1]);
            picture.graphics.lineTo(vertices[indices[i+2]*2],vertices[indices[i+2]*2+1]);
            picture.graphics.lineTo(vertices[indices[i+0]*2],vertices[indices[i+0]*2+1]);
            picture.graphics.endFill();
        }

        flash.Lib.current.addChild(picture);
    }
}
