package neworld;

import Type;

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
