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
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).
%%internal functions
-export([insert/3, lookup/1, delete_obsolete/1]).
-define(SERVER,?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
start_link() ->
  gen_server:start_link({local,?SERVER},?MODULE,[],[]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
init([]) ->
  {_,TableID} = dets:open_file(?DETS_FILE,[]),
  {ok,#my_cache_state{dets_id=TableID}}.

%% @private
%% @doc Handling call messages
handle_call(
    {?INSERT,Key,Value,TimeValue}, _From,
    #my_cache_state{dets_id =Dets_ID}) ->
  Res = my_cache_impl:insert(Dets_ID,{Key,Value,TimeValue+erlang:system_time(?SEC)}),
  {reply, ok,#my_cache_state{dets_id = Dets_ID, result = Res}};
handle_call({?INSERT,Key,Value},From,State) ->
  handle_call({?INSERT,Key,Value,60},From,State);
handle_call({?LOOKUP,Key},_From,#my_cache_state{dets_id=Dets_ID})
  ->
  Result = my_cache_impl:lookup(Dets_ID,Key),
  Return = #my_cache_state{dets_id = Dets_ID,result = Result},
  {reply,Return,#my_cache_state{dets_id=Dets_ID}};
handle_call(_Request,_From,State) ->
  io:format("~w~n",[State]),
  {reply,ok,State}.

%% @private
%% @doc Handling cast messages
handle_cast({?DELETE,Method},#my_cache_state{dets_id = Dets_ID} = State) ->
  spawn(my_cache_impl,?DELETE,[Method,Dets_ID]),
  {noreply, State};
handle_cast(_Request, State) ->
  io:format("~w~n", [State]),
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
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

insert(Key, Value, TimeValue)->
  gen_server:call(?MODULE, {?INSERT, Key, Value, TimeValue}).

lookup(Key)->
  gen_server:call(?MODULE, {?LOOKUP, Key}).

delete_obsolete(Method)->
  gen_server:cast(?MODULE, {?DELETE, Method}).

%%%===================================================================

