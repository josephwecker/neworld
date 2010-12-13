/* Given a player and a map- this renders the actual view
 *
 * ViewPort scroll-type
 * ViewPort bounding box boundaries
 *
 * Player map location (map column, z, tile-offsets)
 * [Player orientation]
 * [Player viewable tiles]
 * 

rendered-map:  (e.g.) 1024 x 768

Dimetric  advance X by: 24
          advance Y by: 12
          offset X by:   4

Isometric advance X by: 34
          advance Y by: 17
          offset X by:  17

Given: map, center-tile, center-height, and viewport width+height
Traverse to top-left-most tile.
Traverse to top as if height were not a factor
Offset Y down based on how high center-height is.  In the future fill this area
  with fuzzed tiles, or paralexing sky, etc...
Move as left as necessary to be >= than 1/2 of column width (advance-X) to the
  left of the viewport boundary.

Like reading a book.  Left to right, top to bottom.
  Render current row
    Render current column
      Offset Y temporarily by height (vs. center-height)
      Render (or have it render itself, or via(?) interface "TileRenderer"...)
        Render surface tile
	Render vertical faces (in future, only as far down as necessary - for
          now just no further than lower boundary of viewport)
      Advance X by x-advance to the right.
      Loop next column to the right unless X is past right boundary
  Back at the ranch (leftmost tile of current row)...
  Advance Y
  If Y is now below boundary, finished.
  Adjust X by offset
  Go down-left one tile
  If current X is more than X-advance to the left of boundary, advance right
    one tile.
  Loop
  

 * 
 * 
 * 
 */

import Player;
import Map;
import flash.display.Bitmap;

class ViewPort extends flash.display.Bitmap {
    //var view : flash.display.Sprite;

    //var width : Float;
    //var height : Float;
    var map : Map;
    var vp : flash.display.BitmapData;
    var renderer : TileRenderer; // = new TileRenderer();

    public var render_rectangle : flash.geom.Rectangle;
    public var render_point : flash.geom.Point;

    public function new() { //main_view) {
        super();
        //view = main_view;
        flash.Lib.current.addChild(this);
        addEventListener(flash.events.Event.ENTER_FRAME, draw_current_state);
        stage.addEventListener(flash.events.Event.RESIZE, calculate_boundaries);
        stage.addEventListener(flash.events.KeyboardEvent.KEY_DOWN, maybe_fullscreen);
        calculate_boundaries(null);
        render_rectangle = new flash.geom.Rectangle(0,0,stage.width,stage.height);
        render_point = new flash.geom.Point(0,0);
        renderer = new TileRenderer();
        vp = new flash.display.BitmapData(Std.int(stage.width), Std.int(stage.height));
        bitmapData = vp;
    }

    function calculate_boundaries(_) {
        width = stage.stageWidth;
        height = stage.stageHeight;
        //vp.width = stage.stageWidth;
        //vp.height = stage.stageHeight;
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
        vp.copyPixels(renderer.render(this), render_rectangle, render_point, null, null, true);
        //graphics.clear();
        //var rendered = renderer.render();


    }
}

class TileRenderer {
    public function new() {

    }

    public function render(vp : ViewPort) {
        var bm = new flash.display.BitmapData(300,300,0x447799);
        return bm;
    }
}

/*class Blob
{
   var mArena:BitmapData;
   var mBits:BitmapData;
   var mRect:Rectangle;
   var mPoint:Point;
   // The "hotspot" is the logical origin of the object, with respect
   //  to the top left of its bitmap rectangle.  This allows you to deal
   //  with the position of the object, without having to worry about drawing
   //  offsets etc.
   var mHotX:Float;
   var mHotY:Float;
 
   // Passing the arena into the constructor is not really required,
   //  but doing this reduces the number of params we have to pass into
   //  the Draw function;
   public function new(inArena:BitmapData,inBits:BitmapData,inX:Int, inY:Int, inW:Int, inH:Int,
           ?inHotX:Null<Float>, ?inHotY:Null<Float>)
   {
      mArena = inArena;
      mBits = inBits;
      mRect = new Rectangle(inX,inY,inW,inH);
      mPoint = new Point(0,0);
      // If null is provided, assume the centre.
      mHotX = inHotX==null ? inW/2 : inHotX;
      mHotY = inHotY==null ? inH/2 : inHotY;
   }
   public function draw(inX:Float,inY:Float)
   {
      mPoint.x = inX-mHotX;
      mPoint.y = inY-mHotY;
      mArena.copyPixels(mBits,mRect,mPoint,null,null,true);
   }
}*/


class ColumnTile {
    public var x_advance : Int;
    public var y_advance : Int;
    public var x_offset  : Int;
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


