%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    NMEA 2000 packet analyzer
%%% @end
%%% Created :  8 Sep 2015 by Tony Rogvall <tony@rogvall.se>

-module(nmea_2000_packet).

-include_lib("can/include/can.hrl").
-include("../include/nmea_2000.hrl").

-export([collect_packet/3]).


collect_packet(#can_frame { id=ID, len=Len, data=Data}, Fun, Dict) ->
    H = nmea_2000_lib:decode_canid(ID band ?CAN_EFF_MASK),
    collect_packet({H,Len,Data}, Fun, Dict);
collect_packet({{Prio,PGN,Src,Dst},Len,Data}, Fun, Dict) ->
    case nmea_2000_pgn:is_small(PGN) of
	true ->
	    P = #nmea_packet { pgn = PGN,
			       order = 0,
			       index = 0,
			       prio = Prio,
			       src = Src,
			       dst = Dst,
			       len = Len,
			       totlen = Len,
			       data = [Data] },
	    fun_packet(Fun, P),
	    dict:store({Src,PGN}, P, Dict);
	false ->
	    case Data of
		<<Order:3,0:5,PLen,PayLoad/binary>> ->
		    P = #nmea_packet { pgn = PGN,
				       order = Order,
				       index = 0,
				       prio = Prio,
				       src = Src,
				       dst = Dst,
				       len = Len-2,
				       totlen = PLen,
				       data = [PayLoad]},
		    fun_packet(Fun, P),
		    dict:store({Src,PGN}, P, Dict);
		<<Order:3,Index:5,PayLoad/binary>> ->
		    PrevIndex = Index-1,
		    case dict:find({Src,PGN}, Dict) of
			error ->
			    io:format("warning: pgn ~w:~w not found\n", 
				      [Src,PGN]),
			    %% warning and drop !
			    Dict;
			{ok,P=#nmea_packet{order=Order,index=PrevIndex}} ->
			    Data1 = [PayLoad|P#nmea_packet.data],
			    Len1 = P#nmea_packet.len + (Len-1),
			    P1 = P#nmea_packet { index = Index, 
						 len=Len1, data=Data1 },
			    fun_packet(Fun, P1),
			    dict:store({Src,PGN}, P1, Dict);
			{ok,_P} ->
			    io:format("warning: pgn ~w:~w, packet lost ~w\n",
				      [Src,PGN,PrevIndex+1]),
			    %% warning and drop !
			    Dict
		    end
	    end
    end.

fun_packet(Fun, P = #nmea_packet{len=Len, totlen=TLen, data=Data}) when Len >= TLen ->
    case list_to_binary(lists:reverse(Data)) of
	<<PayLoad:TLen/binary, _/binary>> ->
	    Fun(P#nmea_packet { data=PayLoad });
	PayLoad ->
	    io:format("packet to short? ~p\n", [PayLoad])
    end;
fun_packet(_Fun, _P) ->
    ok.
