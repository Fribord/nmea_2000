%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2015, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%% NMEA 2000 application api.
%%%
%%% File: nmea_200.erl <br/>
%%% Created:  9 Sep 2015 by Tony Rogvall
%%% @end
%%%-------------------------------------------------------------------
-module(nmea_2000).

-include_lib("can/include/can.hrl").
-include("../include/nmea_2000.hrl").

-export([start/0]).

-export([file/1]).
-export([join/1]).
-export([input/1]).
-export([text_expand/2]).

%% Test api
-export([test/1]).

-define(SERVER, nmea_2000_srv).

start() ->
    application:start(can),
    application:start(nmea_2000).

-define(dbg(F,A), ok).
%% -define(dbg(F,A), io:format((F),(A)).

%%--------------------------------------------------------------------
%% @doc
%% Open a connection to the nmea server.
%% @end
%%--------------------------------------------------------------------
-spec join(Params::list(term())) -> 
			{ok, Id::term()} | 
			{error, Error::term()}.

join(Params) ->
    gen_server:call(?SERVER, {join, self(), Params}).

%%--------------------------------------------------------------------
%% @doc
%% Send data as a can_frame to the nmea server.
%% @end
%%--------------------------------------------------------------------
-spec input(Frame::#can_frame{}) -> ok.

input(Frame) when is_record(Frame, can_frame) ->
    gen_server:cast(?SERVER, {input, self(), Frame}).

%%--------------------------------------------------------------------
%% @doc
%% Process nmea data from a file.
%% @end
%%--------------------------------------------------------------------
-spec file(Frame::string()) -> ok | 
			       {error, Error::term()}.

file(File) ->
    {ok,Out} = file:open("nmea.log", [write]),
    R = 
	file(File, 
	     fun(P) ->
		     ?dbg("src:~w pgn:~w totlen=~w, len=~w data=~p\n", 
			  [P#nmea_packet.src,P#nmea_packet.pgn,
			   P#nmea_packet.totlen,P#nmea_packet.len,
			   P#nmea_packet.data]),
		     try nmea_2000_pgn:decode(P#nmea_packet.pgn,
					      P#nmea_packet.data) of
			 false ->
			     emit(Out,"PGN:~w, data=~w\n",
				  [P#nmea_packet.pgn,
				   P#nmea_packet.data]);
			 Params ->
			     emit(Out,"~1024p.\n", [Params])
		     catch
			 error:Reason ->
			     emit(Out, "pgn:~w ~p, crash data=~p\n~p\n", 
				  [P#nmea_packet.pgn,
				   Reason,
				   P#nmea_packet.data,
				   erlang:get_stacktrace()])
		     end
	     end),
    file:close(Out),
    R.

emit(Fd, Fmt, Args) ->
    io:format(Fmt, Args),
    io:format(Fd, Fmt, Args).

    
file(File, Fun) ->
    case nmea_2000_log:open(File) of
	{ok, Fd} ->
	    try loop(Fd, Fun) of
		ok -> ok
	    catch
		error:Reason ->
		    io:format("crash: ~p\n", 
			      [erlang:get_stacktrace()]),
		    {error, Reason}
	    after
		nmea_2000_log:close(Fd)
	    end;
	Error ->
	    Error
    end.

loop(Fd, Fun) ->
    loop(Fd, Fun, dict:new()).

loop(Fd, Fun, Dict) ->
    case nmea_2000_log:read_can_frame(Fd) of
	eof -> ok;
	CanFrame = #can_frame {} ->
	    Dict1 = nmea_2000_packet:collect_packet(CanFrame, Fun, Dict),
	    loop(Fd, Fun, Dict1)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Process nmea data from a file and send to nmea_2000_srv
%% @end
%%--------------------------------------------------------------------
-spec test(Frame::string()) -> ok | 
			       {error, Error::term()}.

test(File) ->
    case nmea_2000_log:open(File) of
	{ok, Fd} ->
	    try test_loop(Fd) of
		ok -> ok
	    catch
		error:Reason ->
		    io:format("crash: ~p\n", 
			      [erlang:get_stacktrace()]),
		    {error, Reason}
	    after
		nmea_2000_log:close(Fd)
	    end;
	Error ->
	    Error
    end.

test_loop(Fd) ->
    case nmea_2000_log:read_can_frame(Fd) of
	eof -> ok;
	CanFrame = #can_frame {} ->
	    nmea_2000:input(CanFrame),
	    timer:sleep(100), %% Do not flood
	    test_loop(Fd)
    end.

%%
%% Utility to exand environment "variables" in unicode text
%% variables are written as ${var} where var is a encoded atom
%% operating system enviroment is accessed through $(VAR)
%% and application library dir $/app/
%%
text_expand(Text, Env) when is_list(Text) ->
    %% assume unicode character list!
    text_expand_(Text, [], Env);
text_expand(Text, Env) when is_binary(Text) ->
    %% assume utf8 encoded data!
    text_expand_(unicode:characters_to_list(Text), [], Env).

text_expand_([$$,${|Text], Acc, Env) ->
    text_expand_collect_(Text, [], [${,$$], env, Acc, Env);
text_expand_([$$,$(|Text], Acc, Env) ->
    text_expand_collect_(Text, [], [$(,$$], shell, Acc, Env);
text_expand_([$$,$/|Text], Acc, Env) ->
    text_expand_collect_(Text, [], [$/,$$], lib, Acc, Env);
text_expand_([$\\,C|Text], Acc, Env) ->
    text_expand_(Text, [C|Acc], Env);
text_expand_([C|Text], Acc, Env) ->
    text_expand_(Text, [C|Acc], Env);
text_expand_([], Acc, _Env) ->
    lists:reverse(Acc).


text_expand_collect_([$)|Text], Var, _Pre, shell, Acc, Env) ->
    case os:getenv(rev_variable(Var)) of
	false ->
	    text_expand_(Text, Acc, Env);
	Value ->
	    Acc1 = lists:reverse(Value, Acc),
	    text_expand_(Text, Acc1, Env)
    end;
text_expand_collect_([$/|Text], Var, _Pre, lib, Acc, Env) ->
    try erlang:list_to_existing_atom(rev_variable(Var)) of
	App ->
	    case code:lib_dir(App) of
		{error,_} ->
		    text_expand_(Text, Acc, Env);
		Value ->
		    Acc1 = lists:reverse(Value, Acc),
		    text_expand_(Text, Acc1, Env)
	    end
    catch
	error:_ ->
	    text_expand_(Text, Acc, Env)
    end;
text_expand_collect_([$}|Text], Var, _Pre, env, Acc, Env) ->
    try erlang:list_to_existing_atom(rev_variable(Var)) of
	Key ->
	    case lists:keyfind(Key, 1, Env) of
		false ->
		    text_expand_(Text, Acc, Env);
		{_,Val} ->
		    Value = lists:flatten(io_lib:format("~w", [Val])),
		    Acc1 = lists:reverse(Value, Acc),
		    text_expand_(Text, Acc1, Env)
	    end
    catch
	error:_ ->
	    text_expand_(Text, Acc, Env)
    end;
text_expand_collect_([C|Text], Var, Pre, Shell, Acc, Env) ->
    if C >= $a, C =< $z;
       C >= $A, C =< $Z;
       C >= $0, C =< $9;
       C =:= $_; C =:= $@;
       C =:= $\s; C =:= $\t -> %% space and tab allowed in begining and end
	    text_expand_collect_(Text, [C|Var], Pre, Shell, Acc, Env);
       true ->
	    %% char not allowed in variable named
	    text_expand_(Text,  [C | Var ++ Pre ++ Acc], Env)
    end;
text_expand_collect_([], Var, Pre, _Shell, Acc, Env) ->
    text_expand_([],  Var ++ Pre ++ Acc, Env).

rev_variable(Var) ->
    trim_hd(lists:reverse(trim_hd(Var))).

trim_hd([$\s|Cs]) -> trim_hd(Cs);
trim_hd([$\t|Cs]) -> trim_hd(Cs);
trim_hd(Cs) -> Cs.
