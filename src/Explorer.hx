import Map;
import Player;

class Explorer {
    var map :      Map;
    var player :   Player;
    var viewport : ViewPort;

    static public function main() {
        haxe.Log.trace = function(v:Dynamic,?i){flash.Lib.trace("Explorer: "+v);};
        var main_explorer = new Explorer();
    }

    public function new() {
        UserInput.initialize();

        map =      new Map();
        player =   new Player();
        viewport = new ViewPort(player, map);
        // Put player on map
        // Connect player and map to viewport
    }
}
