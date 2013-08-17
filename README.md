# WebSocket RPC application #

## Intro ##

This library enables to communicate with 'gen_server's via WebSocket or 
HTTP using JSON formatted messages. It is supposed that callback module 
implementing 'gen_server' behavior uses erlang records to represent a 
messages passed to 'gen_server:handle_call'.

## Plug your service into WSRPC ##

To make your gen_server's callable via 'wsrpc' you must:
 
 * implement resolver module
 * configure 'wsrpc' application in a configuration file
 * provide record mapping info

The resolver is a module with a function:
    :::erlang
    -spec resolve(ServicePath :: [ binary() ]) -> {gen_server, pid()} .

Actually this function may reside in any module you want, the only purpose 
of it to know how to find | activate | instantiate a gen_server process.

The second step is to make 'wsrpc' know about your gen_server's i.e. about 
your resolver. For example let we have an application 'my_app' in which we 
have a resolver 'my_services', then the configuration section may look 
like following:

     :::erlang
     {wsrpc, [	{http_port, 8484}, 
     	     	{apps, [
			{my_app, [ {services, [my_services]} ] } 
		       ] }
	     ] },

The third step is described in a next section.

## Provide record mapping information ##

WSRPC require some help from you to be able to map record to JSON and back. 
You must support a special additional call in your 'gen_server':

    :::erlang
    handle_call({get_type, RecordName}, _From, State) ->
    	{reply, [field1, field2, field3] , State};

I.e. your server must be ready to reply a field list for requested record 
name if your server uses this record in its calls. All records in use, 
will be mapped to/from JSON in a strict way. For example let we have a 
record like:

       :::erlang
       #message{ id = 0, operation = "call_smth_useful", args = ["data1", "data2"]}

then it will be mapped to JSON as:

     :::erlang
     { "id"        : 0,
       "type"      : "message",
       "operation" : "call_smth_useful", 
       "args"      : [ "data1", "data2" ] }

and vise versa.

## WebSocket mode ##

TBD

