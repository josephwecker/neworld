package experiments.newexplorer;

import experiments.newexplorer.ViewPort;
import experiments.newexplorer.world.World;

class Explorer {
    var vp : ViewPort;
    public static function main() {
        haxe.Log.trace = function(v:Dynamic,?i){flash.Lib.trace("Explorer: "+v+"\n");};
        var main_obj = new Explorer();
    }

    public function new() {
        var world = new World();
        var vp = new ViewPort(world);
        //vp.render_from(world.columns[100][100], D.NORTHEAST);
        vp.render_from(world.columns[100][100], D.NORTH);
    }
}
