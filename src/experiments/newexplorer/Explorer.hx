package experiments.newexplorer;

import experiments.newexplorer.ViewPort;
import experiments.newexplorer.world.World;
import experiments.newexplorer.world.Column;

import de.polygonal.core.event.IObserver;
import de.polygonal.core.event.Observable;
import de.polygonal.core.time.Timebase;
import de.polygonal.core.time.TimebaseEvent;

class Explorer implements IObserver {
    public var key :Int;
    var vp         :ViewPort;
    var dir        :Int;
    var world      :World;
    var curr_col   :Column;

    public static function main() {
        haxe.Log.trace = function(v:Dynamic,?i){flash.Lib.trace("Explorer: "+v+"\n");};
        var main_obj = new Explorer();
    }

    public function new() {
        key = 0;
        world = new World();
        vp = new ViewPort(world);
        D.orientation = D.NORTHEAST;
        curr_col = world.columns[75][75];
        flash.Lib.current.stage.addEventListener(flash.events.KeyboardEvent.KEY_DOWN, rotate);

        rotate(null);
        //update(0, null, null);
        //Timebase.attach(this, TimebaseEvent.RENDER);
    }

    public function update(type : Int, source : Observable, data : Dynamic) {
        //curr_col = curr_col.n[D.rel(D.UP)];
        //vp.render_from(curr_col);
    }

    function rotate(e :flash.events.KeyboardEvent) {
        if(e != null) {
            var s = String.fromCharCode(e.charCode);
            if(s == 'L') D.orientation = D.rel(D.RIGHT);
            else if(s == 'H') D.orientation = D.rel(D.LEFT);
            else if(s == 'h') curr_col = curr_col.n[D.rel(D.LEFT)];
            else if(s == 'k') curr_col = curr_col.n[D.rel(D.UP)];
            else if(s == 'j') curr_col = curr_col.n[D.rel(D.DOWN)];
            else if(s == 'l') curr_col = curr_col.n[D.rel(D.RIGHT)];
            else if(s == 'y') curr_col = curr_col.n[D.rel(D.UPLEFT)];
            else if(s == 'u') curr_col = curr_col.n[D.rel(D.UPRIGHT)];
            else if(s == 'b') curr_col = curr_col.n[D.rel(D.DOWNLEFT)];
            else if(s == 'n') curr_col = curr_col.n[D.rel(D.DOWNRIGHT)];
        }

        vp.render_from(curr_col);
    }
}
