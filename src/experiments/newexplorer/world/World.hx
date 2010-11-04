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
        for(layer in map.data) {
            trace("Layer - "+layer.getPixel(100,100));
        }
    }


}
