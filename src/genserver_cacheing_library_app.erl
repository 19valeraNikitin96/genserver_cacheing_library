-module(genserver_cacheing_library_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	genserver_cacheing_library_sup:start_link()
.

stop(_State) ->
	ok.
