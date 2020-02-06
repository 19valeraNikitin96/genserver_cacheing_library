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

-record(lifetime, {initial_time,value,unit}).
