#! /bin/sh

env ERL_LIBS=..:deps erl -config etc/app.config -sname wsrpc -s wsrpc_app
