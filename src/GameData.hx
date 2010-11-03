
class GameData {
    static var RESOURCES = [
        'earth'];

    public function new() {
        preprocess_data();
    }

    function preprocess_data() {
        for(type in RESOURCES) {
            process_file(type);
        }
    }

    function process_file(filekey) {

    }
}
