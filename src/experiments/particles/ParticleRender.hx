package experiments.particles;

import noise.Colors;
import noise.Random;

class P {public static var rng = new Prng(Std.int(Math.random() * 10000));}

typedef Node = {x :Float, y :Float, h_x :Float, h_y :Float, w :Float, h :Float};

class ParticleRender extends flash.display.Sprite {
    static public function main() {
        haxe.Log.trace = function(v:Dynamic,?i){flash.Lib.trace("Particles: "+v);};
        var main = new ParticleRender();
    }

    public function new() {
        super();
        flash.Lib.current.stage.addEventListener
            (flash.events.KeyboardEvent.KEY_DOWN, render_it);
        //render_it();
    }

    function render_it(_) {
        var orig :HSL = {h:90.0, s:0.5, l:0.2};
        var base :HSL = {h: 0.0, s:0.0, l:0.0};
        var nodes = new Array<Node>();
        for(n in 0...10) {
            var new_x = 100 + P.rng.norm(150);
            var new_y = 100 + P.rng.norm(150);
            nodes[n] = {
                x: new_x,
                y: new_y,
                h_x: new_x,
                h_y: new_y,
                w: 120 + P.rng.norm(5),
                h: 120 + P.rng.norm(5)}
        }

        for(i in 0...3500) {
            for(n2 in 0...3) {
                var node = nodes[n2];
                var curr_x = node.x + (node.w / 2.0) + P.rng.norm(node.w / 2.5 - (i * 0.015));
                var curr_y = node.y + (node.h / 2.0) + P.rng.norm(node.h / 2.5 - (i * 0.015));

                var ydiff = curr_y - node.h_y;
                var xdiff = curr_x - node.h_x;
                var norm_dist = Math.sqrt((ydiff * ydiff) + (xdiff * xdiff));

                //trace('------' + norm_dist);
                
                base.h = orig.h - (0.002   * i);
                base.s = orig.s + (0.00001 * i);
                //base.l = orig.l /*+ (0.0001  * i)*/ - norm_dist;
                base.l = orig.l + (0.00007 * i);
                if(base.l < 0.08) base.l = 0.08;
                var particle = new Particle(base, (node.w + node.h) / 20);

                particle.x = curr_x;
                particle.y = curr_y;

                addChild(particle);
            }
        }
        flash.Lib.current.addChild(this);
    }
}

class Particle extends flash.display.Shape {
    public function new(base_color :HSL, base_scale) {
        var sc = base_scale; // (for brevity)
        trace("Starting up.");
        super();
        //graphics.beginFill(rc(base_color));
        var new_l = base_color.l + P.rng.norm(0.3);
        var fill_matrix = new flash.geom.Matrix();
        fill_matrix.createGradientBox(sc, sc, Math.random() * 3.1415 * 4);
        graphics.beginGradientFill(
                flash.display.GradientType.LINEAR,
                [rc(base_color),
                 rc({h:base_color.h,
                     s:base_color.s,
                     l:new_l})],
                //[rc(base_color), rc(base_color), 0xff0000],
                [1.0, 0.95 + P.rng.norm(0.05)],
                [0x00, 0xff],
                fill_matrix);
        graphics.moveTo (0.8 * sc,     0);
        graphics.curveTo(rf(sc),       0,            rf(sc),       rf(0.1 * sc));
        graphics.curveTo(rf(sc),       rf(0.3 * sc), rf(0.8 * sc), rf(0.3 * sc));
        graphics.curveTo(rf(0.7 * sc), rf(0.4 * sc), rf(0.7 * sc), rf(0.1 * sc));
        graphics.curveTo(rf(0.5 * sc), 0,            rf(0.7 * sc), 0);
        graphics.endFill();
        var matrix = transform.matrix;
        matrix.rotate(Math.random() * 3.1415 * 8);
        matrix.scale(1 + P.rng.norm(0.3), 1 + P.rng.norm(0.3));
        transform.matrix = matrix;
    }

    function rf(num :Float, ?mag :Float) :Float {
        if(mag == null) mag = 0.1;
        var magnitude = num * P.rng.norm(mag);
        return num + magnitude;
    }

    function rc(base_color :HSL) :Int {
        var col = {
            h:rf(base_color.h, 0.15),
            s:rf(base_color.s),
            l:rf(base_color.l, 0.3)
        };
        var rcol = Colors.hsl2rgb(col);
        return Colors.toInt(rcol);
    }
}
