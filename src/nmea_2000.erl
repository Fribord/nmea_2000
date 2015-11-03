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

%% Test api
-export([test/1]).
-export([emit_log/1]).

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
%% Emit CAN frame from log file on CAN bus
%%
emit_log(File) ->
    case nmea_2000_log:open(File) of
	{ok, Fd} ->
	    try emit_can_loop(Fd) of
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
		
emit_can_loop(Fd) ->
    case nmea_2000_log:read_can_frame(Fd) of
	eof -> ok;
	CanFrame ->
	    can:send(CanFrame),
	    timer:sleep(100), %% Do not flood
	    emit_can_loop(Fd)
    end.
