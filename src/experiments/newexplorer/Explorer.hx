package experiments.newexplorer;

import experiments.newexplorer.ViewPort;
import experiments.newexplorer.world.World;

class Explorer {
    var vp : ViewPort;
    var dir : Int;
    var world : World;
    var walk_x : Int;
    var walk_y : Int;
    public static function main() {
        haxe.Log.trace = function(v:Dynamic,?i){flash.Lib.trace("Explorer: "+v+"\n");};
        var main_obj = new Explorer();
    }

    public function new() {
        world = new World();
        vp = new ViewPort(world);
        dir = D.NORTHEAST;
        walk_x = 0;
        walk_y = 0;
        flash.Lib.current.stage.addEventListener(flash.events.KeyboardEvent.KEY_DOWN, rotate);
        rotate(null);
    }

    function rotate(_) {
        //dir = (dir + 2) % 8;
        vp.render_from(world.columns[100 + walk_x][100 - walk_y], dir);
        walk_x += 1;
        //walk_y += 1;
    }
}
