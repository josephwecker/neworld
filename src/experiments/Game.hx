package experiments;

import flash.display.Sprite;
import flash.text.TextField;

class Game extends Sprite {
    var title : TextField;
    var ground : flash.display.Shape;
    var orientation : String;

    //   N
    // NW NE
    //W- + -E
    // SW SE
    //   S
    static var map_n_ = [
        [1,2,3],
        [4,5,6],
        [7,8,9]];
    // BECOMES
    static var map_nw = [
        [0,1,0],
         [4,2],
        [7,5,3],
         [8,6],
        [0,9,0]];
    static var map_w = [
        [7,4,1],
        [8,5,2],
        [9,6,3]];
    static var map_sw = [
        [0,7,0],
         [8,4],
        [9,5,1],
         [6,2],
        [0,3,0]];
    static var map_s = [
        [9,8,7],
        [6,5,4],
        [3,2,1]];
    static var map_se = [
        [0,9,0],
         [6,8],
        [3,5,7],
         [2,4],
        [0,1,0]];
    static var map_e = [
        [3,6,9],
        [2,5,8],
        [1,4,7]];
    static var map_ne = [
        [0,3,0],
         [2,6],
        [1,5,9],
         [4,8],
        [0,7,0]];

    static var map = [
        [0,2,4,6,4,2,0],
        [0,2,4,6,4,2,0],
        [0,2,4,5,3,2,0],
        [0,1,3,3,2,1,0],
        [0,1,2,2,1,1,0],
        [0,1,1,1,1,1,0],
        [0,0,0,0,0,0,0]];

    static public function main() {
        haxe.Log.trace = haxeTrace;
        new Game();
    }

    public static function haxeTrace(v: Dynamic, ?infos : Dynamic) {
        flash.Lib.trace("GAME: " + v);
    }

    public function new() {
        super();
        flash.Lib.current.addChild(this);
        title = new TextField();
        title.text = "prerendering tiles";
        addChild(title);
        flash.Lib.current.addEventListener(flash.events.Event.ENTER_FRAME,onEnterFrame);
        orientation = 'NE';
        draw_map();
    }

    function onEnterFrame(_) {
        //draw_map();
    }

    function draw_map() {
        var tmap = get_map(3,3, 7,7);
        addChild(ground);
    }

    function get_map(mx, my, w, h) {
        var resmap = new Array();
        var xstep; var xstart; var startx;
        var ystep; var ystart; var starty;
        startx = mx;
        starty = my;
        xstep = 1;
        ystep = 1;
        if(orientation == 'NE') {
            var x_xstep = 1;
            var x_ystep = 1;
            var y_xstep = -1;
            startx = mx;
            starty = my - (h >> 1);
        }
        var currx = startx;
        var curry = starty;
        for(ny in 0...h) {
            resmap[ny] = new Array();
            for(nx in 0...w) {
                if(currx < 0 || curry < 0 || currx > map[0].length || curry < map.length) {
                    resmap[ny][nx] = 0;
                } else {
                    resmap[ny][nx] = map[curry][currx];
                }
                currx += xstep;
                curry += ystep;
            }
        }
    }
}
