/*  _   _                         _     _ 
 * | \ | |                       | |   | |
 * |  \| | _____      _____  _ __| | __| |
 * | . ` |/ _ \ \ /\ / / _ \| '__| |/ _` |
 * | |\  |  __/\ V  V / (_) | |  | | (_| |
 * \_| \_/\___| \_/\_/ \___/|_|  |_|\__,_|
 *
 * NEWORLD - SIMULATION / GAME
 * Copyright (c) 2010 Joseph Wecker & Samuel Wecker
 *
 * CONFIDENTIAL!  ALL RIGHTS RESERVED!
 *
 */

package neworld;

import neworld.terrain.World;

class Neworld {
    var world :World;
    public static function main() {
        haxe.Log.trace = function(v:Dynamic,?i){flash.Lib.trace("Neworld: "+v+"\n");};
        var main_obj = new Neworld();
    }

    public function new() {
        world = new World();
    }
}
