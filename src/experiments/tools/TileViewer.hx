package experiments.tools;

import assets.terrain.Display;
import experiments.newexplorer.world.Column;
import experiments.newexplorer.Tiles;
import experiments.newexplorer.ViewPort;

class TileViewer {
    var vp          :ViewPort;
    var curr_col    :Column;

    static public function main() {
        var main_obj = new TileViewer();
    }

    public function new() {
        var w=4;
        var h=2;
        var test_area = [[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]];

        for(x in 0...w) {
            for(y in 0...h) {
                var l = x == 0 ? w - 1 : x - 1;
                var r = x == w - 1 ? 0 : x + 1;
                var u = y == 0 ? h - 1 : y - 1;
                var d = y == h - 1 ? 0 : y + 1;

                test_area[x][y].n[D.NORTH] =     test_area[x][u];
                test_area[x][y].n[D.NORTHEAST] = test_area[r][u];
                test_area[x][y].n[D.EAST] =      test_area[r][y];
                test_area[x][y].n[D.SOUTHEAST] = test_area[r][d];
                test_area[x][y].n[D.SOUTH] =     test_area[x][d];
                test_area[x][y].n[D.SOUTHWEST] = test_area[l][d];
                test_area[x][y].n[D.WEST] =      test_area[l][y];
                test_area[x][y].n[D.NORTHWEST] = test_area[l][u];
            }
        }
 
        vp = new ViewPort(test_area, 5, 3);

    }

}
