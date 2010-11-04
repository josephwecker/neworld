package experiments.newexplorer;

import experiments.newexplorer.world.World;

class Explorer {
    public static function main() {
        haxe.Log.trace = function(v:Dynamic,?i){flash.Lib.trace("Explorer: "+v);};
        var main_obj = new Explorer();
    }

    public function new() {
        var world = new World();
    }
}
