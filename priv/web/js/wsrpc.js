var wsrpc = {};

wsrpc.Service = function(conf){
    this.status_listener = conf.status_listener;
    this.calls = {};
    var calls = this.calls;
    this.next_rid = 0;

    //this.url = document.URL.replace("http","ws") + conf.path;
    if(conf.url != null)
	this.url = conf.url;
    else
	throw("no_url");

    if ("MozWebSocket" in window) 
    {
        WebSocket = MozWebSocket;
    }
    if ("WebSocket" in window) {
        // browser supports websockets
        var ws = new WebSocket(this.url);
	this.ws = ws;
        ws.onopen = function() {
            // websocket is connected
            conf.status_listener("connected");
        };
	
	ws.onmessage = function (evt) {
//console.log(msg_text);
            var msg = JSON.parse(evt.data);
	    var rid = msg.rid;
	    if(rid == null) 
		throw("No request ID (RID): " + msg);
	    
	    calls[rid].onreply(msg.data);
	    
	    if(msg.type == "stream-end") 
		delete calls[rid];
	};
	ws.onclose = function() {
            // websocket was closed
            conf.status_listener("disconnected"); 
	};
    } else {
	// browser does not support websockets
	conf.status_listener("sorry, your browser does not support websockets.");
    }
}

wsrpc.Service.prototype.call = function(call, onreply){
    var status_listener = this.status_listener;
    if(onreply == null)
	onreply = function(ReplyData){
	    status_listener(ReplyData)
	};
    var rid = this.next_rid++;
    this.calls[rid] = {onreply:onreply, type:"call"};
    var rpc_req = {type:"call", rid:rid, data:call};
    this.ws.send(JSON.stringify(rpc_req));
}
