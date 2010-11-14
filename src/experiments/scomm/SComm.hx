package experiments.scomm;

import flash.net.XMLSocket;
import flash.events.Event;
import flash.events.DataEvent;
import flash.events.IOErrorEvent;
import flash.events.ProgressEvent;
import flash.events.SecurityErrorEvent;

class SComm {
    var socket :XMLSocket;

    public static function main() {
        haxe.Log.trace = function(v:Dynamic,?i){flash.Lib.trace("SComm: "+v);};
        var main = new SComm();
    }

    public function new() {
        socket = new XMLSocket();
        socket.addEventListener(Event.CLOSE,    on_socket_close);
        socket.addEventListener(Event.CONNECT,  on_socket_connect);
        socket.addEventListener(DataEvent.DATA, on_socket_data);
        socket.addEventListener(IOErrorEvent.IO_ERROR, on_socket_io_error);
        socket.addEventListener(ProgressEvent.PROGRESS, on_socket_progress);
        socket.addEventListener(SecurityErrorEvent.SECURITY_ERROR, on_socket_security_error);
        socket.connect('mongodb2.justin.tv', 4113);
        socket.send("Hello server");
    }

    function on_socket_close(e :Event)          {trace("CLOSE "+e);}
    function on_socket_connect(e :Event)        {trace("CONNECT "+e);}
    function on_socket_io_error(e :Event)       {trace("IO-ERROR "+e);}
    function on_socket_progress(e :Event)       {trace("PROGRESS "+e);}
    function on_socket_security_error(e :Event) {trace("SECURITY-ERROR "+e);}
    
    function on_socket_data(e :DataEvent) {
        trace("DATA: "+ e.data);
    }
}
