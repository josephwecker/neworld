package noise;
import noise.Simplex;

class RidgedSimplex extends Simplex {
    var weight :Float;

    override function new(?seed=123, ?octaves=4, ?falloff=0.5, ?lacunarity=2.0) {
        super(seed, octaves, falloff, lacunarity);
        weight = 1.0;
    }

    override inline function noise_transform(noise, octave, freq, pers) {
        if(oct > 0) {
            noise = Math.abs(noise);
            noise = 0.745 - noise;
            noise = noise * noise;
        }
        noise *= weight;
        weight = noise * 1.05;
        if(weight > 1) weight = 1;
        else if(weight < 0) weight = 0;
        return noise * freq;
    }

    override inline function record_point(bitmap, noise, px, py) {
        var color :Int = Std.int(val * 128);
        if(color > 255) color = 255;
        else if(color < 0) color = 0;
        if(color < 24) bitmap.setPixel( px, py, color * 3 + 24);
        else if(color > (256 - 32)) bitmap.setPixel( px, py, ((color-32) << 16)
                + ((color-32) << 8) + (color-32) );
        else {
            bitmap.setPixel(px, py, (color << 16));
        }
        weight = 1.0;
    }

}
