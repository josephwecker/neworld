package experiments.explorer;
import experiments.explorer.Map;

class Player {
    public function new() {

    }
}

enum Direction {
    UP_LEFT;     UP;     UP_RIGHT;
    LEFT;        NONE;   RIGHT;
    DOWN_LEFT;   DOWN;   DOWN_RIGHT;
}

class UserInput {
    static var _instance : UserInput;
    public static var keys = new Array<Bool>();

    public static function initialize() {getInstance();}

    public static function getInstance() {
        if(_instance == null) _instance = new UserInput();
        return _instance;
    }

    public static function moving() : Direction {
        var x = 0;
        var y = 0;
        if(_left())      x -= 1;
        if(_up())        y -= 1;
        if(_down())      y += 1;
        if(_right())     x += 1;

        if(x == 0) {
            if(y == 0) return NONE;
            if(y < 0)  return UP;
                       return DOWN;
        } else if(x < 0) {
            if(y == 0) return LEFT;
            if(y < 0)  return UP_LEFT;
                       return DOWN_LEFT;
        } else {
            if(y == 0) return RIGHT;
            if(y < 0)  return UP_RIGHT;
                       return DOWN_RIGHT;
        }
    }

    public static inline function _left() : Bool {
        return keys[37] || // LEFT
               keys[72] || // h
               keys[36] || // HOME (keypad)
               keys[35] || // END (keypad)
               keys[89] || // y
               keys[66];   // b
    }

    public static inline function _up() : Bool {
        return keys[38] || // UP
               keys[75] || // k
               keys[89] || // y
               keys[85] || // u
               keys[36] || // HOME (keypad)
               keys[33];   // PGUP (keypad)
    }

    public static inline function _right() : Bool {
        return keys[39] || // RIGHT
               keys[76] || // l
               keys[85] || // u
               keys[78] || // n
               keys[33] || // PGUP (keypad)
               keys[34];   // PGDOWN (keypad)
    }

    public static inline function _down() : Bool {
        return keys[40] || // DOWN
               keys[66] || // b
               keys[78] || // n
               keys[74] || // j
               keys[34] || // PGDOWN (keypad)
               keys[35];   // END (keypad)
    }

    private function new() {
        flash.Lib.current.stage.addEventListener(
            flash.events.KeyboardEvent.KEY_DOWN,
            on_key_down);
        flash.Lib.current.stage.addEventListener(
            flash.events.KeyboardEvent.KEY_UP,
            on_key_up);
    }

    function on_key_down(key_event : flash.events.KeyboardEvent) {
        keys[key_event.keyCode] = true;
    }

    function on_key_up(key_event : flash.events.KeyboardEvent) {
        keys[key_event.keyCode] = false;
    }
}
