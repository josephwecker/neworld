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
        var vertices_array  = new Array<Float>();
        var indices_array   = new Array<Int>();
        var picture         = new flash.display.Shape();


        var hm = [ 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 
                   00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 
                   00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 
                   00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 
                   00, 00, 00, 00, 00, 00, 00, 50, 00, 00, 00, 00, 00, 00, 00, 
                   00, 00, 00, 00, 00, 00, 50, 60, 50, 00, 00, 00, 00, 00, 00, 
                   00, 20, 20, 20, 20, 50, 60, 70, 60, 50, 00, 00, 00, 00, 00, 
                   00, 00, 00, 00, 00, 00, 50, 60, 50, 00, 00, 00, 00, 00, 00, 
                   00, 00, 00, 00, 00, 00, 00, 50, 00, 30, 00, 00, 00, 00, 00, 
                   00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 20, 20, 20, 20, 20, 
                   00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 
                   00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 
                   00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 
                   00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 
                   00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00];
       

        var width = 24;
        var height = 12;
        var col_count = 14;
        var row_count = 14;
        for( count in 0...((row_count+1)*(col_count+1)) ) {
            vertices_array.push(count%(col_count+1)*width+50);
            vertices_array.push(Math.floor(count/(col_count+1))*height-hm[count]+50);
        }

        /*vertices_array =
            [100.0,000.0,  150.0,000.0,  200.0,000.0,  250.0,000.0,  300.0,000.0,  350.0,000.0,  400.0,000.0,
             100.0,050.0,  150.0,050.0,  200.0,050.0,  250.0,050.0,  300.0,050.0,  350.0,050.0,  400.0,050.0,
             100.0,100.0,  150.0,100.0,  200.0,100.0,  250.0,100.0,  300.0,100.0,  350.0,100.0,  400.0,100.0,
             100.0,150.0,  150.0,140.0,  200.0,130.0,  250.0,130.0,  300.0,140.0,  350.0,150.0,  400.0,150.0,
             100.0,200.0,  150.0,200.0,  200.0,190.0,  250.0,190.0,  300.0,200.0,  350.0,200.0,  400.0,200.0,
             100.0,250.0,  150.0,250.0,  200.0,250.0,  250.0,250.0,  300.0,250.0,  350.0,250.0,  400.0,250.0,
             100.0,300.0,  150.0,300.0,  200.0,300.0,  250.0,300.0,  300.0,300.0,  350.0,300.0,  400.0,300.0,
             100.0,350.0,  150.0,350.0,  200.0,350.0,  250.0,350.0,  300.0,350.0,  350.0,350.0,  400.0,350.0,
             100.0,400.0,  150.0,400.0,  200.0,400.0,  250.0,400.0,  300.0,400.0,  350.0,400.0,  400.0,400.0];
        */
        var row_tick = true;
        var col_tick = true;
        for( row in 0...row_count ) {
            col_tick = true;
            for( col in 0...col_count) {
                if( row_tick == col_tick ) {
                    indices_array.push(row*(col_count+1)+col);
                    indices_array.push((row+1)*(col_count+1)+col);
                    indices_array.push((row+1)*(col_count+1)+col+1);
                    indices_array.push(row*(col_count+1)+col);
                    indices_array.push(row*(col_count+1)+col+1);
                    indices_array.push((row+1)*(col_count+1)+col+1);
                } else {
                    indices_array.push(row*(col_count+1)+col+1);
                    indices_array.push(row*(col_count+1)+col);
                    indices_array.push((row+1)*(col_count+1)+col);
                    indices_array.push(row*(col_count+1)+col+1);
                    indices_array.push((row+1)*(col_count+1)+col+1);
                    indices_array.push((row+1)*(col_count+1)+col);
                }
                col_tick = !col_tick;
            }
            row_tick = !row_tick;
        }
        
        
        vertices = vertices_array.toFloatVector();
        indices  = indices_array.toIntVector();

        picture.graphics.lineStyle(0.25,0xffffff);
        for( tri in 0...(Math.floor(indices.length/3)) ) {
            var i = tri*3;
            picture.graphics.beginFill(0x449944);
            picture.graphics.moveTo(vertices[indices[i+0]*2],vertices[indices[i+0]*2+1]);
            picture.graphics.lineTo(vertices[indices[i+1]*2],vertices[indices[i+1]*2+1]);
            picture.graphics.lineTo(vertices[indices[i+2]*2],vertices[indices[i+2]*2+1]);
            picture.graphics.lineTo(vertices[indices[i+0]*2],vertices[indices[i+0]*2+1]);
            picture.graphics.endFill();
        }

        flash.Lib.current.addChild(picture);
    }
}
