import format.pbj.Data;

class TestShader {

    static function main() {
        var bytes = initPBJ();
        // creates a shader from PBJ bytes
        var shader = new flash.display.Shader(bytes.getData());
        // add one shape
        var root = flash.Lib.current;
        var a = new flash.display.Shape();
        a.graphics.beginFill(0xFFFF00);
        a.graphics.drawCircle(50,50,100);
        root.addChild(a);
        // add one second shape
        var b = new flash.display.Shape();
        b.graphics.beginFill(0x00FFFF);
        b.graphics.drawCircle(120,120,100);
        root.addChild(b);
        // use our shader to perform Multiply blendMode
        b.blendMode = flash.display.BlendMode.SHADER;
        b.blendShader = shader;
    }

    static function initPBJ() {
        var pbj : PBJ = {
            version : 1,
            name : "Multiply",
            metadatas : [],
            // the parameters are the input/output of the shader
            // see PBJ Reference below for a full description
            parameters : [
                { name : "_OutCoord", p : Parameter(TFloat2,false,RFloat(0,[R,G])), metas : [] },
                { name : "background", p : Texture(4,0), metas : [] },
                { name : "foreground", p : Texture(4,1), metas : [] },
                { name : "dst", p : Parameter(TFloat4,true,RFloat(1)), metas : [] },
            ],
            // this is our assembler code for the shader, you can see it's similar
            // to what we have written in previous section
            code : [
                OpSampleNearest(RFloat(2),RFloat(0,[R,G]),0),
                OpSampleNearest(RFloat(1),RFloat(0,[R,G]),1),
                OpMul(RFloat(1),RFloat(2)),
            ],
        };
        var output = new haxe.io.BytesOutput();
        var writer = new format.pbj.Writer(output);
        writer.write(pbj);
        return output.getBytes();
    }

}
