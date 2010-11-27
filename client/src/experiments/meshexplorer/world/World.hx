package experiments.meshexplorer.world;

import experiments.meshexplorer.world.SampleMap;
import experiments.meshexplorer.world.Column;
import experiments.meshexplorer.ViewPort;

class World {
    var map : SampleMap;
    public var columns : Array<Array<Column>>;

    public function new() {
        trace("Beginning world creation");
        map = new SampleMap();
        columns = new Array<Array<Column>>();
        create_world();
        trace("Finish world creation");
    }

    function create_world() {
        var w = map.data[0].width;
        var h = map.data[0].height;

        for(x in 0...w) {
            columns[x] = new Array<Column>();
            for(y in 0...h) {
                columns[x][y] = new Column(map.data, x, y);
            }
        }

        // Link them together (at some point this will be done lazily)
        for(x in 0...w) {
            for(y in 0...h) {
                var l = x == 0 ? w - 1 : x - 1;
                var r = x == w - 1 ? 0 : x + 1;
                var u = y == 0 ? h - 1 : y - 1;
                var d = y == h - 1 ? 0 : y + 1;

                columns[x][y].n[D.NORTH] =     columns[x][u];
                columns[x][y].n[D.NORTHEAST] = columns[r][u];
                columns[x][y].n[D.EAST] =      columns[r][y];
                columns[x][y].n[D.SOUTHEAST] = columns[r][d];
                columns[x][y].n[D.SOUTH] =     columns[x][d];
                columns[x][y].n[D.SOUTHWEST] = columns[l][d];
                columns[x][y].n[D.WEST] =      columns[l][y];
                columns[x][y].n[D.NORTHWEST] = columns[l][u];
            }
        }
        //trace(columns[100][100].describe());
    }
}
