%%%-------------------------------------------------------------------
%%% @author erlang
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Feb 2020 12:23
%%%-------------------------------------------------------------------
-module(my_cache).
-author("erlang").
-include("header.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3, delete_obsolete/1]).

-define(SERVER, ?MODULE).

-record(my_cache_state, {dets_id}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
init([]) ->
  {_,TableID} = dets:open_file("cache_dets.file", []),
  {ok, #my_cache_state{dets_id = TableID}}.

%% @private
%% @doc Handling call messages
handle_call(
    {insert, Key, Value, TimeValue}, From,
    #my_cache_state{dets_id = Dets_ID} = State) ->
  dets:insert(Dets_ID,{Key,Value,TimeValue+erlang:system_time(?SEC)}),
  Result = {reply, ok,State},
  From!Result,
  Result;

handle_call({insert,Key,Value,#lifetime{value = TimeValue,unit = TimeUnit}},
            From,State) ->
  case TimeUnit of
       ?SEC -> handle_call({insert,Key,Value,TimeValue}, From, State);
       ?MIN -> handle_call({insert,Key,Value,TimeValue*60}, From, State)
  end;

handle_call({lookup, Key},From,#my_cache_state{dets_id = Dets_ID} = State)
  ->delete_obsolete(Dets_ID),
  Res = {reply,ok, State,dets:lookup(Dets_ID, Key)},
  From!Res,
  Res;

handle_call(_Request, _From, State) ->
  io:format("~w~n", [State]),
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
handle_cast({clear}, #my_cache_state{dets_id = Dets_ID} = State) ->
  spawn(my_cache, delete_obsolete, [Dets_ID]),
  {noreply, State}
;
handle_cast(_Request, State) ->
  io:format("~w~n", [State]),
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #my_cache_state{}) ->
  {noreply, NewState :: #my_cache_state{}} |
  {noreply, NewState :: #my_cache_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #my_cache_state{}}).
handle_info(_Info, State = #my_cache_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
terminate(_Reason, #my_cache_state{dets_id = Dets_ID}) ->
  dets:close(Dets_ID),
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, State = #my_cache_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

all_records_from(Dets_ID) ->
  Temp_Ets_ID = ets:new(temp_ets, []),
  dets:to_ets(Dets_ID, Temp_Ets_ID),
  Result = ets:tab2list(Temp_Ets_ID),
  ets:delete(Temp_Ets_ID),
  Result.

delete_obsolete(Dets_ID)->
  delete_obsolete(all_records_from(Dets_ID), Dets_ID).
delete_obsolete([], _Dets_ID)-> ok;
delete_obsolete([{_,_,EndTime}=H|T], Dets_ID) ->
  case erlang:system_time(?SEC) > EndTime of
    true -> dets:delete_object(Dets_ID, H), delete_obsolete(T, Dets_ID);
    false -> delete_obsolete(T, Dets_ID)
  end.