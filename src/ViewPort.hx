/* Given a player and a map- this renders the actual view
 *
 * ViewPort scroll-type
 * ViewPort bounding box boundaries
 *
 * Player map location (map column, z, tile-offsets)
 * [Player orientation]
 * [Player viewable tiles]
 * 
 * 
 * 
 * 
 */

import Player;


class ColumnTile {
    public function new() {}
}

class Dimetric extends ColumnTile {
    public function new() {
        super();
        x_advance = 24;
        y_advance = 12;
        x_offset  = 3;
    }
}

class Isometric extends ColumnTile {
    public function new() {
        super();
        x_advance = 34;
        y_advance = 17;
        x_offset  = 17;
    }
}

class ViewPort {
    var view : flash.display.Sprite;

    var width : Float;
    var height : Float;

    public function new(main_view) {
        view = main_view;
        view.addEventListener(flash.events.Event.ENTER_FRAME, draw_current_state);
        view.stage.addEventListener(flash.events.Event.RESIZE, calculate_boundaries);
        view.stage.addEventListener(flash.events.KeyboardEvent.KEY_DOWN, maybe_fullscreen);
        calculate_boundaries(null);
    }

    function draw_current_state(_) {
        view.graphics.clear();
        // TODO: render
    }

    function calculate_boundaries(_) {
        width = view.stage.stageWidth;
        height = view.stage.stageHeight;
    }

    function maybe_fullscreen(e : flash.events.KeyboardEvent) {
        if(e.keyCode == 70) {
            trace("Toggling fullscreen");
            if(view.stage.displayState == flash.display.StageDisplayState.NORMAL)
                view.stage.displayState = flash.display.StageDisplayState.FULL_SCREEN;
            else view.stage.displayState = flash.display.StageDisplayState.NORMAL;
        }
    }
}

