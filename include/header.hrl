%%%-------------------------------------------------------------------
%%% @author erlang
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Feb 2020 12:17
%%%-------------------------------------------------------------------
-author("erlang").

-define(NULL, undefined).

%%TIME UNITS
-define(SEC, 'second').
-define(MIN, 'minute').
-define(TIME_UNITS, [?SEC, ?MIN]).

-record(lifetime, {value,unit}).

%%OPERATIONS
-define(CREATE, create).
-define(INSERT, insert).
-define(LOOKUP, lookup).
-define(DELETE, delete_obsolete).

%%CLEAR METHODS
-define(STANDARD, clear_standard).
-define(NONSTANDARD, clear_using_iterator).

%%STATE
-define(DETS_FILE, "cache_dets.file").
-record(my_cache_state, {dets_id, result}).
