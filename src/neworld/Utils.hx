package neworld;

class Utils {
    inline public static function toFloatVector(arr:Array<Float>) :flash.Vector<Float> {
        var output = new flash.Vector<Float>(arr.length, true);
        for( i in 0...arr.length ) output[i]=arr[i];
        return output;
    }

    inline public static function toIntVector(arr:Array<Int>) :flash.Vector<Int> {
        var output = new flash.Vector<Int>(arr.length, true);
        for( i in 0...arr.length ) output[i]=arr[i];
        return output;
    }
    
    inline public static function toUIntVector(arr:Array<UInt>) :flash.Vector<UInt> {
        var output = new flash.Vector<UInt>(arr.length, true);
        for( i in 0...arr.length ) output[i]=arr[i];
        return output;
    }
}

class H<T> implements Dynamic<T>, extends Hash<T> {
    public function new(?in_arr) {
        super();
        if(in_arr != null) fromArray(in_arr);
    }

    public function fromArray(init_array:Array<Array<Dynamic>>) {
        for(item in init_array) set(item[0], item[1]);
    }

    function resolve(name:String) : T {return get(name);}
}

class U {
    public static function rand(max) {
        return Math.floor(Math.random() * max);
    }

    public static function randInt() : UInt {
        return U.rand(0xFFFFF);
    }
}
