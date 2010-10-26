import flash.display.BitmapData;
import flash.display.Bitmap;
import Utils;

/* Extends sprite for all the events etc., but never actually displays anything
 * directly- only has main_bitmap display to the screen.
 */
class ViewPort extends flash.display.Sprite {
    var portal_width(get_portal_width, null) : Int;
    var portal_height(get_portal_height, null) : Int;
    var view_data : BitmapData;
    var main_bitmap : Bitmap;

    public function new() {
        super();

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

        addEventListener(flash.events.Event.ENTER_FRAME, draw_current_state);
        stage.addEventListener(flash.events.Event.RESIZE, calculate_boundaries);
        stage.addEventListener(flash.events.KeyboardEvent.KEY_DOWN, maybe_fullscreen);
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

    function draw_current_state(_) {
        view_data.lock();
        view_data.noise(U.randInt());
        view_data.unlock();
    }
}

