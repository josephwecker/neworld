import Map;
import Player;

class Explorer extends flash.display.Sprite {
    var map :      Map;
    var player :   Player;
    var viewport : ViewPort;

    static public function main() {
        haxe.Log.trace = function(v:Dynamic,?i){flash.Lib.trace("Explorer: "+v);};
        var main_explorer = new Explorer();
    }

    public function new() {
        super();
        flash.Lib.current.addChild(this);

        cacheAsBitmap =   true;
        stage.scaleMode = flash.display.StageScaleMode.NO_SCALE;
        stage.align =     flash.display.StageAlign.TOP_LEFT;
        contextMenu =     null;
        mouseEnabled =    false;

        UserInput.initialize();

        map =      new Map();
        player =   new Player();
        viewport = new ViewPort(this);
        // Put player on map
        // Connect player and map to viewport

        //mc.addEventListener(flash.events.Event.ENTER_FRAME, on_frame);
        // Movement key events
    }

    function on_frame(e) {
        //player.move(); ---> in player class instead?
        //viewport.refresh();
    }
}
