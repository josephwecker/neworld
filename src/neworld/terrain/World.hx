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

import noise.Simplex;
import noise.RidgedPerlin;
import de.polygonal.core.math.random.ParkMiller;
import neworld.terrain.Column;

/**
 * Responsible for building the physical terrain of the world, extendable in
 * any direction.  Possibly also in charge of swapping in and out the right
 * segments as needed by other parts of the simulation.
 *
 * Terrain creation basics:
 *   [Continent]Simplex(?)() *
 *     HydrolicErosion(ThermalErosion(
 *        (Simplex() * FaultLine()) +
 *        (Simplex2() * Ridged()) +
 *        (Simplex3() * Clamper() * Ridged())))
 *   Several layers / Strata, then weather, then water.
 *
 * Scales:
 *   Continent  [65,536m | 262,144t](1024 | 2^10)
 *   Land(?)    [ 4,096m |  16,384t](  64 | 2^ 6)
 *   ActiveView [    64m |     256t](   1 | 2^ 0)
 *   Viewport   [ ~  32m |     128t](   1 | 2^ 0)
 *
 * @todo
 *   - Initial point?
 */
class World {

    /**
     * @param world_number A number between 0 and 0x7FFFFFFE (inclusive) that
     * is essentially the random seed for all the physical world building
     * algorithms.
     */
    public function new(world_number=null) {
        if(seed == null) seed = Std.int(Math.random() * 0x7FFFFFF);

    }

    /**
     * Gets the definition of the new column relative to the reference column.
     * For getting a column when the normal linked list value is null.
     *
     * Among other things, it gives the new column a set of coordinates,
     * automatically incrementing higher scaled ones when necessary.
     */
    public function get_col(reference_col :Column, direction :Int) {

    }


}

/**
 * Singleton class that handles orientation and directions, relative and
 * absolute.
 */
class D {
    static var _instance = new D();
    var _orientation :Int;
    public static var orientation(get_orientation, set_orientation) :Int;
    public static var tile_type(get_tile_type, null) :TileType;

    public inline static var NORTH     = 0;
    public inline static var NORTHEAST = 1;
    public inline static var EAST      = 2;
    public inline static var SOUTHEAST = 3;
    public inline static var SOUTH     = 4;
    public inline static var SOUTHWEST = 5;
    public inline static var WEST      = 6;
    public inline static var NORTHWEST = 7;

    public inline static var UP        = 8;
    public inline static var UPRIGHT   = 9;
    public inline static var RIGHT     = 10;
    public inline static var DOWNRIGHT = 11;
    public inline static var DOWN      = 12;
    public inline static var DOWNLEFT  = 13;
    public inline static var LEFT      = 14;
    public inline static var UPLEFT    = 15;
    private function new() {_orientation = NORTH;}
    static function get_tile_type() :TileType {
        if(_instance._orientation & 1 == 1) return isometric;
        else return dimetric;
    }
    static function get_orientation() :Int {return _instance._orientation;}
    static function set_orientation(o:Int) :Int {_instance._orientation = o;return o;}
}

