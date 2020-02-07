%%%-------------------------------------------------------------------
%%% @author erlang
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Feb 2020 15:42
%%%-------------------------------------------------------------------
-module(my_cache_tests).
-author("erlang").

-include_lib("eunit/include/eunit.hrl").

first_test()->
  my_cache:init([]),
  ?assertEqual({reply,ok,{my_cache_state,"cache_dets.file"}}, my_cache:handle_call({insert, 11,"Valerii",0}, self(), {my_cache_state,"cache_dets.file"}))
.

second_test()->
  my_cache:init([]),
  ?assertEqual({reply,ok,{my_cache_state,"cache_dets.file"}}, my_cache:handle_call({insert, 111,"Valerii",0}, self(), {my_cache_state,"cache_dets.file"}))
.

third_test()->
  my_cache:init([]),
  {_,ok,{my_cache_state,"cache_dets.file"}, List} = my_cache:handle_call({lookup, 111}, self(), {my_cache_state,"cache_dets.file"}),
  ?assert(length(List) == 1)
.

fourth_test()->
  my_cache:init([]),
  ?assertEqual({reply,ok,{my_cache_state,"cache_dets.file"}}, my_cache:handle_call({insert, 111,"Valerii", -1}, self(), {my_cache_state,"cache_dets.file"})),
  {_,ok,{my_cache_state,"cache_dets.file"}, List} = my_cache:handle_call({lookup, 111}, self(), {my_cache_state,"cache_dets.file"}),
  ?assertEqual(true, length(List)==0)
.

%%my_cache:handle_call({lookup,111},self(), {my_cache_state,"cache_dets.file"}).
%%my_cache:handle_call({insert, 111, "Valera"},self(),{my_cache_state,"cache_dets.file"}).
%%my_cache:handle_call({lookup},self(),{my_cache_state,"cache_dets.file"}).
%%my_cache:handle_cast({clear},{my_cache_state,"cache_dets.file"}).