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

-record(my_cache_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #my_cache_state{}} | {ok, State :: #my_cache_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {_,TableID} = dets:open_file("cache_dets.file", []),
%%  TODO some risk of exception present
  io:format("~w~n", [{{dets_id, TableID}}]),
  {ok, {TableID}}.

%% @private
%% @doc Handling call messages

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #my_cache_state{}) ->
  {reply, Reply :: term(), NewState :: #my_cache_state{}} |
  {reply, Reply :: term(), NewState :: #my_cache_state{}, timeout() | hibernate} |
  {noreply, NewState :: #my_cache_state{}} |
  {noreply, NewState :: #my_cache_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #my_cache_state{}} |
  {stop, Reason :: term(), NewState :: #my_cache_state{}}).
%%insert(Key, Value, TimeValue, Unit)->
%%  insert(Key, Value, #lifetime{initial_time = erlang:system_time(?SEC), value = TimeValue, unit = Unit})
%%.
handle_call({insert, Key, Value, TimeValue, Unit}, From, State)
  ->{Dets_ID} = State
  , dets:insert(Dets_ID,{Key,Value,#lifetime{initial_time = erlang:system_time(?SEC), value = TimeValue, unit = Unit}})
  , From!{reply, ok,State}
  , {reply,ok,State}
;
handle_call({insert, Key, Value, {lifetime,_InitialTime,_TimeValue,_Unit}=R}, From, State)
  ->{Dets_ID} = State
  , dets:insert(Dets_ID,{Key,Value,R})
  , From!{reply, ok,State}
  , {reply,ok,State}
;
handle_call({insert, Key, Value, TimeValue}, From, State) ->
  handle_call(
    { insert,
      Key,
      Value,
      #lifetime
      {initial_time = erlang:system_time(?SEC),
        value = TimeValue,
        unit = ?SEC
      }
    },
    From,
    State
  );
handle_call({insert, Key, Value}, From, State) ->
  handle_call(
    { insert,
      Key,
      Value,
      #lifetime
      {initial_time = erlang:system_time(?SEC),
        value = 60,
        unit = ?SEC
      }
    },
    From,
    State
  );
handle_call({lookup}, From, State)
  ->{Dets_ID} = State
  , io:format("~w~n",[all_records_from(Dets_ID)])
  , From ! {reply, ok,State}
  , {reply, ok,State}
;
handle_call({lookup, Key}, From, State)
  ->{Dets_ID} = State,
    delete_obsolete(Dets_ID),
    Res = {reply,ok, State, dets:lookup(Dets_ID, Key)},
    From!Res,
    Res
;
handle_call(_Request, _From, State = #my_cache_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #my_cache_state{}) ->
  {noreply, NewState :: #my_cache_state{}} |
  {noreply, NewState :: #my_cache_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #my_cache_state{}}).

handle_cast({clear}, State) ->
  {Dets_ID} = State,
  spawn(my_cache, delete_obsolete, [Dets_ID]),
  {noreply, State}
;
handle_cast(_Request, State = #my_cache_state{}) ->
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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #my_cache_state{}) -> term()).
terminate(_Reason, _State = #my_cache_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #my_cache_state{},
    Extra :: term()) ->
  {ok, NewState :: #my_cache_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #my_cache_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

all_records_from(Dets_ID)
  ->Temp_Ets_ID = ets:new(temp_ets, [])
  , dets:to_ets(Dets_ID, Temp_Ets_ID)
  , Result = ets:tab2list(Temp_Ets_ID),
  ets:delete(Temp_Ets_ID),
  Result
.
delete_obsolete(Dets_ID)->
  delete_obsolete(all_records_from(Dets_ID), Dets_ID)
.
delete_obsolete([], _Dets_ID)-> ok;
delete_obsolete([{_,_,{_,InitialTime, Value, _Unit}}=H|T], Dets_ID) ->
  case erlang:system_time(?SEC) > InitialTime+Value of
    true -> dets:delete_object(Dets_ID, H), delete_obsolete(T, Dets_ID);
    false -> delete_obsolete(T, Dets_ID)
  end
.
%%my_cache:handle_call({lookup,111}, {[99,97,99,104,101,95,100,101,116,115,46,102,105,108,101]}).
%%my_cache:handle_call({insert, 111, "Valera"},self(),{[99,97,99,104,101,95,100,101,116,115,46,102,105,108,101]}).
%%my_cache:handle_call({lookup},self(),{[99,97,99,104,101,95,100,101,116,115,46,102,105,108,101]}).