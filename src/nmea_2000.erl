%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%
%%% @end
%%% Created :  9 Sep 2015 by Tony Rogvall <tony@rogvall.se>

-module(nmea_2000).

-include_lib("can/include/can.hrl").
-include("../include/nmea_2000.hrl").

-export([start/0]).
-export([file/1]).

start() ->
    application:start(can),
    application:start(nmea_2000).

-define(dbg(F,A), ok).
%% -define(dbg(F,A), io:format((F),(A)).

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
