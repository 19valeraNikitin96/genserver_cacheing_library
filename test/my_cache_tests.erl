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
-include("header.hrl").
-include_lib("eunit/include/eunit.hrl").

first_test()->
  my_cache:start_link(),
  ?assertEqual(ok,my_cache:insert(11,"Valerii",0)).

second_test()->
  my_cache:start_link(),
  my_cache:insert(11,"Valerii",-1),
  ?assertEqual({my_cache_state,"cache_dets.file",{ok,[]}},my_cache:lookup(11)).

third_test()->
  my_cache:start_link(),
  my_cache:insert(11,"Valerii",-1),
  my_cache:delete_obsolete(?STANDARD),
  ?assertEqual({my_cache_state,"cache_dets.file",{ok,[]}},my_cache:lookup(11)).

fourth_test()->
  my_cache:start_link(),
  my_cache:insert(11,"Valerii",-1),
  my_cache:delete_obsolete(?NONSTANDARD),
  ?assertEqual({my_cache_state,"cache_dets.file",{ok,[]}},my_cache:lookup(11)).