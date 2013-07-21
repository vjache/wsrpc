///////////// Init /////////////////////////////////////////                    
var  assert = require('assert'), util = require('util'), wsrpc = require('wsrpc');



///////////// Tests /////////////////////////////////////////                   
describe('Service', function(){
    var svc;
    before(function(done){
	svc = new wsrpc.Service(
	    {url: "ws://127.0.0.1:8585/wsrpc/wsrpc_gs_resolver",
	     status_listener: function(status){
		 assert.equal('connected', status);
		 done()
	     }
	    })
    })

    it('should call echo without error', function(done){
	var cnt = 0;
	var callEcho = function(msg){
	    svc.call({type:'echo', text:msg}, function(Reply){
		assert.equal(msg + msg, Reply.text);
		cnt++;
	    });
	}
	callEcho("Hello!");
	callEcho("Buy!");
	setTimeout(function(){
	    assert.equal(2, cnt);
	    done()
	}, 50)
    })

    it('should call timer without error', function(done){
	var cnt = 0;
	var callTimer = function(per, lab){
	    svc.call({type:'timer', period:per, label:lab}, function(Reply){
		util.log(Reply);
		assert.equal(lab, Reply);
		cnt++;
	    });
	}
	callTimer(20, "tick");
	callTimer(20, "tack");
	setTimeout(function(){
	    assert.notEqual(0, cnt);
	    done()
	}, 500)
    })
})
