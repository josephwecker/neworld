package experiments.explorer;

import de.polygonal.core.event.IObserver;
import de.polygonal.core.event.Observable;
import de.polygonal.core.time.Timebase;
import de.polygonal.core.time.TimebaseEvent;
import flash.display.BitmapData;
import flash.display.Bitmap;
import neworld.Utils;

/* Extends sprite for all the events etc., but never actually displays anything
 * directly- only has main_bitmap display to the screen.
 *
 * Disables context menu as much as possible and, handles resize events, and
 * handles the rendering loop.
 */
class ViewPort extends flash.display.Sprite,
               implements IObserver {
    var portal_width(get_portal_width, null) : Int;
    var portal_height(get_portal_height, null) : Int;
    var view_data : BitmapData;
    var main_bitmap : Bitmap;
    var copy_point : flash.geom.Point;
    public var key : Int;
    var projection : Projection;

    public function new(player, map) {
        super();

        key = 1;
        flash.Lib.current.addChild(this);
        flash.Lib.current.cacheAsBitmap =   true;
        stage.scaleMode =                   flash.display.StageScaleMode.NO_SCALE;
        stage.align =                       flash.display.StageAlign.TOP_LEFT;
        stage.showDefaultContextMenu=       false;
        flash.Lib.current.contextMenu =     new flash.ui.ContextMenu();
        flash.Lib.current.contextMenu.hideBuiltInItems();
        flash.Lib.current.mouseEnabled =    false;

        main_bitmap = new Bitmap(null, flash.display.PixelSnapping.ALWAYS, false);
        calculate_boundaries(null);
        flash.Lib.current.addChild(main_bitmap);
        projection = new Projection(player, map);
        copy_point = new flash.geom.Point(0,0);

        Timebase.attach(this, TimebaseEvent.RENDER);
        stage.addEventListener(flash.events.Event.RESIZE, calculate_boundaries);
        stage.addEventListener(flash.events.KeyboardEvent.KEY_DOWN, maybe_fullscreen);
    }

    public function update(type : Int, source : Observable, data : Dynamic) {
        trace("Update Render: " + type + " | " + source + " | " + data);
        view_data.lock();
        // DEBUG view_data.noise(U.randInt());
        projection.render(view_data, portal_width, portal_height);
        view_data.unlock();
    }

    inline function get_portal_width() : Int { return stage.stageWidth;}
    inline function get_portal_height(): Int { return stage.stageHeight;}

    function calculate_boundaries(_) {
        view_data = new BitmapData(portal_width, portal_height, true, 0);
        main_bitmap.bitmapData = view_data;
        stage.scaleMode = flash.display.StageScaleMode.NO_SCALE;
        stage.align = flash.display.StageAlign.TOP_LEFT;
        main_bitmap.width = portal_width;
        main_bitmap.height = portal_height;
    }

    function maybe_fullscreen(e : flash.events.KeyboardEvent) {
        try {
            if(e.keyCode == 70) {
                trace("Toggling fullscreen");
                if(stage.displayState == flash.display.StageDisplayState.NORMAL)
                    stage.displayState = flash.display.StageDisplayState.FULL_SCREEN;
                else stage.displayState = flash.display.StageDisplayState.NORMAL;
            }
        } catch(_:Dynamic) {}
    }
}


/* Renders the scene and copies pixels into the bitmap that they want.  Should
 * eventually handle scrolling etc. as well.
 *
 * TODO:
 *  - objectstore for blit objects
 *  - objectstore for tiles
 */

class Projection {
    var player : Player;
    var map    : Map;
    public function new(player, map) {
        this.player = player;
        this.map = map;
    }

    public function render(data, width, height) {

    }
}
