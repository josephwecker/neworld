package experiments;

class Experiment extends flash.display.Sprite {
    var noisy : flash.display.BitmapData;
    var title : flash.text.TextField;
    var frmnum : Int;
    var perl : noise.OptimizedPerlin;

    public function new() {
        super();
        flash.Lib.current.addChild(this);
        noisy = new flash.display.BitmapData(100, 100, true, 0x00FF00);
        perl = new noise.OptimizedPerlin(123, 5, 0.55);
        //perl.fill(noisy, 0, 0, 0);
        addChild(new flash.display.Bitmap(noisy));
        frmnum = 0;
        /*title = new flash.text.TextField();
        title.text = "Hi there!";
        addChild(title);*/
        //var bmd = new flash.display.BitmapData(300, 300, true, 0x000000);
        flash.Lib.current.addEventListener(flash.events.Event.ENTER_FRAME,onEnterFrame);
    }

    function onEnterFrame(e) {
        perl.fill(noisy, frmnum, frmnum, frmnum);
        frmnum ++;
    }

    static public function main() {
        haxe.Log.trace = haxeTrace;
        var m = new Experiment();
    }

    public static function haxeTrace(v: Dynamic, ?infos : Dynamic) {
        flash.Lib.trace("EXPERIMENT: " + v);
    }
}
