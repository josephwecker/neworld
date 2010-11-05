package experiments.newexplorer;

import experiments.newexplorer.ViewPort;
import experiments.newexplorer.world.World;
import experiments.newexplorer.world.Column;

import de.polygonal.core.event.IObserver;
import de.polygonal.core.event.Observable;
import de.polygonal.core.time.Timebase;
import de.polygonal.core.time.TimebaseEvent;

class Explorer implements IObserver {
    public var key : Int;
    var vp : ViewPort;
    var dir : Int;
    var world : World;
    var curr_col : Column;

    public static function main() {
        haxe.Log.trace = function(v:Dynamic,?i){flash.Lib.trace("Explorer: "+v+"\n");};
        var main_obj = new Explorer();
    }

    public function new() {
        key = 0;
        world = new World();
        vp = new ViewPort(world);
        D.orientation = D.NORTHEAST;
        curr_col = world.columns[100][100];
        flash.Lib.current.stage.addEventListener(flash.events.KeyboardEvent.KEY_DOWN, rotate);
        update(0, null, null);
        Timebase.attach(this, TimebaseEvent.RENDER);
    }

    public function update(type : Int, source : Observable, data : Dynamic) {
        curr_col = curr_col.n[D.rel(D.UP)];
        vp.render_from(curr_col);
    }

    function rotate(_) {
        D.orientation = D.rel(D.RIGHT);
    }
}
