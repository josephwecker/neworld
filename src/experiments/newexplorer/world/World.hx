package experiments.newexplorer.world;

import experiments.newexplorer.world.SampleMap;
import experiments.newexplorer.world.Column;

class World {
    var map : SampleMap;
    var columns : Array<Array<Column>>;

    public function new() {
        trace("Beginning world creation");
        map = new SampleMap();
        create_world();
        trace("Finish world creation");
    }

    function create_world() {
        for(px in 0...map.data[0].width) {
            for(py in 0...map.data[0].height) {

            }
        }
    }


}
