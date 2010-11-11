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

package neworld.terrain;

import neworld.terrain.World;
import de.polygonal.ds.mem.ByteMemory;

/**
 * A column of terrain- kind of like a geologist's core-sample.  Basically
 * going straight down, it is a linked list of the various stratus' and their
 * heights in that spot on the world.
 *
 * @todo
 *  - Object pool for its own layer of caching.  Ideally the object pool will
 *    keep track of which ones are the least accessed.
 */
class Column {
    var top_layer : Layer;

    // Position and neighbors
    var world   :World;
    var world_x :Float, world_y :Float;
    var col_x   :UInt,  col_y   :UInt;

    var n_nw    :Column, n_n    :Column, n_ne    :Column;
    var n_w     :Column,                 n_e     :Column;
    var n_sw    :Column, n_s    :Column, n_se    :Column;

    public var total_height (get_total_height, null) : Int;

    public function new(world, world_x, world_y, col_x, col_y) {
        top_layer = null;
    }

    public function next(dir :Int, ?relative_to :Int = null) :Column {
        if(dir > 7) {
            if(relative_to == null) relative_to = D.orientation;
            dir = (dir + relative_to) % 8;
        }
        return switch(dir) {
            case 0: get_n_n();
            case 1: get_n_ne();
            case 2: get_n_e();
            case 3: get_n_se();
            case 4: get_n_s();
            case 5: get_n_sw();
            case 6: get_n_w();
            case 7: get_n_nw();
        }
    }

    inline function get_n_n (){if(n_n ==null) n_n =world.get_col(this, D.NORTH);     return n_n ;}
    inline function get_n_ne(){if(n_ne==null) n_ne=world.get_col(this, D.NORTHEAST); return n_ne;}
    inline function get_n_e (){if(n_e ==null) n_e =world.get_col(this, D.EAST);      return n_e ;}
    inline function get_n_se(){if(n_se==null) n_se=world.get_col(this, D.SOUTHEAST); return n_se;}
    inline function get_n_s (){if(n_s ==null) n_s =world.get_col(this, D.SOUTH);     return n_s ;}
    inline function get_n_sw(){if(n_sw==null) n_sw=world.get_col(this, D.SOUTHWEST); return n_sw;}
    inline function get_n_w (){if(n_w ==null) n_w =world.get_col(this, D.WEST);      return n_w ;}
    inline function get_n_nw(){if(n_nw==null) n_nw=world.get_col(this, D.NORTHWEST); return n_nw;}

    public function to_string() :String {
        return '';
    }

    function get_total_height() : Int {
        var layer = top_layer;
        var res = 0;
        while(layer != null) {
            res += layer.height;
            layer = layer.next_down;
        }
        return res;
    }

    public function move_by(dir, times) {
        var r = this;
        for(i in 0...times) r = r.n[D.rel(dir)];
        return r;
    }
}

class Layer {
    public var substance : TerrainSubstance;
    public var height    : UInt;
    public var next_up   : Layer;
    public var next_down : Layer;

    public function new(height, substance) {
        this.height = height;
        this.substance = substance;
        next_up = null;
        next_down = null;
    }

    public function link_down(next) {
        next_down = next;
        next.next_up = this;
        return next;
    }
}

enum TerrainSubstance {
    bedrock;
    air;
    earth;
}
