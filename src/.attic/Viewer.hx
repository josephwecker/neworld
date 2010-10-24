import sandy.core.Scene3D;

import sandy.core.scenegraph.Group;
import sandy.core.scenegraph.Camera3D;
import sandy.core.data.Vertex;

import sandy.materials.Appearance;
import sandy.materials.Material;
import sandy.materials.ColorMaterial;
import sandy.materials.attributes.LightAttributes;
import sandy.materials.attributes.LineAttributes;
import sandy.materials.attributes.MaterialAttributes;

import sandy.primitive.Plane3D;
import sandy.primitive.PrimitiveMode;

import Map;
import Utils;

class Viewer extends flash.display.Sprite {
    var scene : Scene3D;
    var camera : Camera3D;
    var ground : TerrainPlane;
    var map : Map;

    public static function main() {
        var viewer = new Viewer();
    }

    public function new() {
        super();
        haxe.Log.trace = function(v:Dynamic,?i){flash.Lib.trace("Viewer: "+v);};
        map = new Map();

        var bmp = new flash.display.Bitmap();
        bmp.bitmapData = map.layers[0];
        bmp.x = 10;
        bmp.y = 10;
        addChild(bmp);

        camera = new Camera3D();
        camera.tilt = 45;
        camera.setPosition(0,190,-170);
        var root = create_scene();
        scene = new Scene3D("viewer", this, camera, root);
        addEventListener(flash.events.Event.ENTER_FRAME, render_frame);
        flash.Lib.current.stage.addChild(this);
    }

    function create_scene() : Group {
        var g = new Group();
        ground = new TerrainPlane(map);
        ground.appearance = new Appearance(
            new sandy.materials.WireFrameMaterial(1, 0x333355, 1, null));
        g.addChild(ground);
        return g;
    }

    function render_frame(_) {
        scene.render();
        //ground.rotateX += 1;
        //ground.rotateY += 1;
        ground.rotateY += 4;
        ground.rotateX += U.rand(3) - 1;
    }
}

class TerrainPlane extends Plane3D {
    var map : Map;
    var perlin : Bitmap;
    public function new(map) {
        super(null, 128, 64, 128, 64, Plane3D.ZX_ALIGNED, PrimitiveMode.TRI);
        this.map = map;
        this.perlin = map.layers[0];
        enableBackFaceCulling = false;
        //update_geom();
        parseBMD();
    }

    function update_geom() {
        var i = 0;
        for(v in geometry.aVertex) {
            var ypos:Int = i % 63;
            var xpos:Int = Std.int((i - ypos) / 127);
            v.y = (map.layers[0].getPixel(xpos, ypos) >> 16) / 255 * 50;
            i += 1;
        }
    }
	private function parseBMD() : Void
	{
		var myGeometry:Geometry3D = geometry;
		var i:Int = 0;
		for(myVertex in geometry.aVertex) {
			var new3DY:Int = 0;
			var coordY:Int = i % quality;
			var coordX:Int = Std.int((i - coordY ) / quality);
			coordX = Std.int( (coordX * ( perlin.width / quality )) );
			coordY = Std.int( (coordY * ( perlin.height / quality )) );
			//perlin.setPixel(coordX, coordY, 0xFF0000);
			new3DY = Std.int(getAVG(coordX, coordY, sampleRate));
			myVertex.y = new3DY * magnitude * .00001;
			i++;
			//trace(coordX, coordY);
		}
		updateForGeometryChange(myGeometry, updateNormals, updateBounds);
	}

	private inline function getAVG(coordX:Int, coordY:Int, spread:Int) : Float
	{
		var avg:Float = perlin.getPixel(coordX,
										coordY);

		var i:Int = coordX - spread;
		var m1 = coordX + spread;
		while( i < m1)
		{
			var j:Int = Std.int(coordY - spread);
			var m2 = coordY + spread;
			while( j < m2 )
			{
				if(i >= 0 && i <= perlin.width && j >= 0 && j <= perlin.height)
					avg += perlin.getPixel(i, j);
				j++;
			}
			i++;
		}
		return avg / (spread * spread * 4.);
	}
}
