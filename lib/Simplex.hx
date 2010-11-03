class SimplexNoise {
    var grads : Array<Array<Int>>;
    static inline var grad3 = [[1, 1,0], [-1, 1,0], [1,-1, 0], [-1,-1, 0],
                    [1, 0,1], [-1, 0,1], [1, 0,-1], [-1, 0,-1],
                    [0, 1,1], [0,-1, 1], [0, 1,-1], [0,-1,-1]];

    var perm : Array<Int>;
  static inline var P = [
    151,160,137,91,90,15,131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,
    142,8,99,37,240,21,10,23,190,6,148,247,120,234,75,0,26,197,62,94,252,
    219,203,117,35,11,32,57,177,33,88,237,149,56,87,174,20,125,136,171,
    168,68,175,74,165,71,134,139,48,27,166,77,146,158,231,83,111,229,122,
    60,211,133,230,220,105,92,41,55,46,245,40,244,102,143,54,65,25,63,161,
    1,216,80,73,209,76,132,187,208,89,18,169,200,196,135,130,116,188,159,
    86,164,100,109,198,173,186,3,64,52,217,226,250,124,123,5,202,38,147,118,
    126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,223,183,170,
    213,119,248,152,2,44,154,163,70,221,153,101,155,167,43,172,9,129,22,39,
    253,19,98,108,110,79,113,224,232,178,185,112,104,218,246,97,228,251,
    34,242,193,238,210,144,12,191,179,162,241,81,51,145,235,249,14,239,
    107,49,192,214,31,181,199,106,157,184,84,204,176,115,121,50,45,127,4,
    150,254,138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,
    156,180,151,160,137,91,90,15,131,13,201,95,96,53,194,233,7,225,140,36,
    103,30,69,142,8,99,37,240,21,10,23,190,6,148,247,120,234,75,0,26,197,
    62,94,252,219,203,117,35,11,32,57,177,33,88,237,149,56,87,174,20,125,
    136,171,168,68,175,74,165,71,134,139,48,27,166,77,146,158,231,83,111,229,
    122,60,211,133,230,220,105,92,41,55,46,245,40,244,102,143,54,65,25,63,
    161,1,216,80,73,209,76,132,187,208,89,18,169,200,196,135,130,116,188,
    159,86,164,100,109,198,173,186,3,64,52,217,226,250,124,123,5,202,38,147,
    118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,223,
    183,170,213,119,248,152,2,44,154,163,70,221,153,101,155,167,43,172,9,
    129,22,39,253,19,98,108,110,79,113,224,232,178,185,112,104,218,246,97,
    228,251,34,242,193,238,210,144,12,191,179,162,241,81,51,145,235,249,
    14,239,107,49,192,214,31,181,199,106,157,184,84,204,176,115,121,50,
    45,127,4,150,254,138,236,205,93,222,114,67,29,24,72,243,141,128,
    195,78,66,215,61,156,180];
    
    var octaves : Int;
    var iXOffset : Float;
    var iYOffset : Float;

    static inline function dot(g:Array<Int>, x:Float, y:Float) {
        return g[0]*x + g[1]*y;
    }

    static inline function fastfloor(n:Float):Int {
        return n > 0 ? Std.int(n) : Std.int(n - 1);
    }

    public function new(?seed=211, ?octaves=3, ?falloff=0.4) {
        perm = P;
        grads = grad3;
        this.octaves = octaves;
        iXOffset = seed = Std.int((seed * 16807.0) % 2147483647);
        iYOffset =        Std.int((seed * 16807.0) % 2147483647);
    }

    public function noise2D(bitmap:flash.display.BitmapData) {
        for(py in 0...bitmap.height) {
            for(px in 0...bitmap.width) {
                var val = 0.0;
                var f = 1.0;
                for(oct in 0...octaves) {
                    var x = (px + iXOffset) * f * 0.005;
                    var y = (py + iYOffset) * f * 0.005;
                    val += noisePoint2D(x, y) / f;
                    f = f * 2.5;
                }
                val = val * val;
                var color : Int = Std.int(val * 255);
                //var color : Int = Std.int((val + 1) / 2 * 255);
                if(color > 255) color = 255;
                else if(color < 0) color = 0;
                bitmap.setPixel(px, py, color << 16);
            }
        }
    }

    inline function noisePoint2D(xin:Float, yin:Float) {
        // Noise contributions
        var n0 : Float;
        var n1 : Float;
        var n2 : Float;

        // Skew the input space to determine which simplex cell we're in
        var F2 = 0.366025403784439; //0.5 * (Math.sqrt(3.0)-1.0);
        var s = (xin + yin) * F2; // Hairy factor for 2D
        var i = fastfloor(xin + s);
        var j = fastfloor(yin + s);

        var G2 = 0.211324865405187; //(3.0 - Math.sqrt(3.0)) / 6.0;
        var t = (i + j) * G2;
        var X0 = i - t;  // Unskew the cell origin back to (x,y) space
        var Y0 = j - t;
        var x0 = xin - X0; // The x,y distances from the cell origin
        var y0 = yin - Y0;

        // For the 2D case, the simplex shape is an equilateral triangle.
        // Determine which simplex we're in.
        var i1 : Int;
        var j1 : Int;  // Offsets for second (middle) corner of simplex in (i,j) coords
        if(x0 > y0) { i1 = 1; j1 = 0; } // Lower triangle, XY order: (0, 0)->(1, 0)->(1, 1)
        else        { i1 = 0; j1 = 1; } // Upper triangle, YX order: (0, 0)->(0, 1)->(1, 1)
        
        // A step of (1, 0) in (i,j) means a step of (1-c,-c) in (x,y), and a
        // step of (0, 1) in (i,j) means a step of (-c,1-c) in (x,y), where c =
        // (3-sqrt(3))/6

        var x1 = x0 - i1 + G2; // Offsets for middle corner in (x,y) unskewed coords
        var y1 = y0 - j1 + G2;
        var x2 = x0 - 1.0 + 2.0 * G2; // Offsets for last corner in (x,y) unskewed coords
        var y2 = y0 - 1.0 + 2.0 * G2;

        // Work out the hashed gradient indices of the three simplex corners
        var ii = i & 255;
        var jj = j & 255;
        var gi0 = perm[ii+perm[jj]] % 12;
        var gi1 = perm[ii+i1+perm[jj+j1]] % 12;
        var gi2 = perm[ii+1+perm[jj+1]] % 12;

        // Calculate the contribution from the three corners
        var t0 = 0.5 - x0*x0-y0*y0;
        if(t0<0) n0 = 0.0;
        else {
            t0 *= t0;
            n0 = t0 * t0 * dot(grads[gi0],x0,y0); // (x,y) of grad3 used for 2D gradient
        }

        var t1 = 0.5 - x1*x1-y1*y1;
        if(t1<0) n1 = 0.0;
        else {
            t1 *= t1;
            n1 = t1 * t1 * dot(grads[gi1], x1, y1);
        }

        var t2 = 0.5 - x2 * x2 - y2 * y2;
        if(t2 < 0) n2 = 0.0;
        else {
            t2 *= t2;
            n2 = t2 * t2 * dot(grads[gi2], x2, y2);
        }

        // Add contributions from each corner to get the final noise value.
        // The result is scaled to return values in the interval [-1, 1].
        return 70.0 * (n0 + n1 + n2);
    }
}
