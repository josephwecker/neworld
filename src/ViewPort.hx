import flash.display.BitmapData;
import flash.display.Bitmap;
import Utils;

/* Extends sprite for all the events etc., but never actually displays anything
 * directly- only has main_bitmap display to the screen.
 */
class ViewPort extends flash.display.Sprite {
    var view_data : BitmapData;
    var main_bitmap : Bitmap;

    public function new() {
        super();

        flash.Lib.current.addChild(this);
        flash.Lib.current.cacheAsBitmap =   true;
        stage.scaleMode =                   flash.display.StageScaleMode.NO_SCALE;
        stage.align =                       flash.display.StageAlign.TOP_LEFT;
        flash.Lib.current.contextMenu =     null;
        flash.Lib.current.mouseEnabled =    false;

        addEventListener(flash.events.Event.ENTER_FRAME, draw_current_state);
        stage.addEventListener(flash.events.Event.RESIZE, calculate_boundaries);
        stage.addEventListener(flash.events.KeyboardEvent.KEY_DOWN, maybe_fullscreen);

        view_data = new BitmapData(stage.stageWidth, stage.stageHeight, true, 0);
        main_bitmap = new Bitmap(view_data);
        flash.Lib.current.addChild(main_bitmap);
    }

    function calculate_boundaries(_) {
        main_bitmap.width = stage.stageWidth;
        main_bitmap.height = stage.stageHeight;
        view_data = new BitmapData(stage.stageWidth, stage.stageHeight, true, 0);
        main_bitmap.bitmapData = view_data;
    }

    function maybe_fullscreen(e : flash.events.KeyboardEvent) {
        if(e.keyCode == 70) {
            trace("Toggling fullscreen");
            if(stage.displayState == flash.display.StageDisplayState.NORMAL)
                stage.displayState = flash.display.StageDisplayState.FULL_SCREEN;
            else stage.displayState = flash.display.StageDisplayState.NORMAL;
        }
    }

    function draw_current_state(_) {
        view_data.lock();
        view_data.noise(U.randInt());
        view_data.unlock();
    }
}

