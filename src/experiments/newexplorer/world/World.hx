package experiments.newexplorer.world;

import experiments.newexplorer.world.SampleMap;
import experiments.newexplorer.world.Column;

class World {
    var map : SampleMap;
    var columns : Array<Array<Column>>;

    public function new() {
        trace("Beginning world creation");
        map = new SampleMap();
        columns = new Array<Array<Column>>();
        create_world();
        trace("Finish world creation");
    }

    function create_world() {
        for(px in 0...map.data[0].width) {
            columns[px] = new Array<Column>();
            for(py in 0...map.data[0].height) {
                //row.push(new Column(map.data, px, py));
                columns[px][py] = new Column(map.data, px, py);
            }
            //columns.push(row);
        }
        trace(columns[100][100].describe());
    }
}
