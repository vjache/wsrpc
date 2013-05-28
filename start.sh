#! /bin/sh

env ERL_LIBS=..:deps erl -sname wsrpc -s wsrpc_app
