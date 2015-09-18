%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Read NMEA log files
%%% @end
%%% Created :  7 Sep 2015 by Tony Rogvall <tony@rogvall.se>

-module(nmea_2000_log).

-include_lib("can/include/can.hrl").

-export([open/1, close/1]).
-export([read/1, read_can_frame/1]).

-compile(export_all).

-define(is_digit(X), (((X) >= $0) andalso ((X) =< $9))).

%% log style 1
%%
%%  <0x01234abcd> [L] D1 D2 D3 D4 D5 D6 D7 D8
%%
%% log style 2
%%  2009-06-18Z09:46:01.129:prio,pgn,src,dst,len,D1,D2,D3,D4,D5,D6,D7,D8
%%
%%

open(File) ->
    file:open(File, [read,binary]).

close(Fd) ->
    file:close(Fd).

read_can_frame(Fd) ->
    case read(Fd) of
	eof -> eof;
	{{Pri,PGN,Src,Dst},Len,FrameData} ->
	    ID = nmea_2000_lib:encode_canid(Pri,PGN,Src,Dst) bor ?CAN_EFF_FLAG,
	    #can_frame { id = ID,
			 len = Len,
			 data = FrameData }
    end.

read(Fd) ->
    case file:read_line(Fd) of
	eof -> eof;
	{ok,Line} ->
	    Sz = byte_size(Line)-1,
	    case Line of
		<<Line1:Sz/binary,$\n>> ->
		    parse(Fd,Line1);
		_ ->
		    parse(Fd,Line)
	    end
    end.

parse(Fd, <<$#,_/binary>>) -> read(Fd);
parse(Fd, <<$\n>>) -> read(Fd);
parse(Fd, <<$\s,Cs/binary>>) -> parse(Fd, Cs);
parse(Fd, <<$\t,Cs/binary>>) -> parse(Fd, Cs);
parse(Fd, <<"interface =",_/binary>>) -> read(Fd);
parse(_Fd, <<$<,$0,$x,LogLine1/binary>>) -> parse_log_1(LogLine1);
parse(_Fd, LogLine2= <<Y1,Y2,Y3,Y4,$-,_/binary>>) when
      ?is_digit(Y1),?is_digit(Y2),?is_digit(Y3),?is_digit(Y4) ->
    parse_log_2(LogLine2);
parse(_Fd, LogLine3) -> parse_log_3(LogLine3).

parse_log_1(Line) ->
    case binary:split(Line, <<" ">>, [global,trim_all]) of
	[<<ID:8/binary,$> >>, <<$[,L,$]>>, D1,D2,D3,D4,D5,D6,D7,D8] ->
	    CanID = erlang:binary_to_integer(ID,16),
	    Len = erlang:list_to_integer([L],16),
	    FrameData = list_to_binary(
			  [ erlang:binary_to_integer(D,16) || 
			      D <- [D1,D2,D3,D4,D5,D6,D7,D8]]),
	    {nmea_2000_lib:decode_canid(CanID),Len,FrameData};
	_Other ->
	    io:format("read log_1: ~p\n", [_Other]),
	    error
    end.

parse_log_3(Line) ->
    case binary:split(Line, <<" ">>, [global,trim_all]) of
	[_Intf, <<ID:8/binary>>, <<$[,L,$]>>, D1,D2,D3,D4,D5,D6,D7,D8] ->
	    CanID = erlang:binary_to_integer(ID,16),
	    Len = erlang:list_to_integer([L],16),
	    FrameData = list_to_binary(
			  [ erlang:binary_to_integer(D,16) || 
			      D <- [D1,D2,D3,D4,D5,D6,D7,D8]]),
	    {nmea_2000_lib:decode_canid(CanID),Len,FrameData};
	_Other ->
	    io:format("read log_1: ~p\n", [_Other]),
	    error
    end.

%% fixme handle more data formats keep time stamp?
parse_log_2(<<Y1,Y2,Y3,Y4,$-,Mon1,Mon2,$-,Day1,Day2,$Z,  %% $T?
	      H1,H2,$:,M1,M2,$:,S1,S2,$.,T1,T2,T3,$,,Data/binary>>) ->
    Year = list_to_integer([Y1,Y2,Y3,Y4]),
    Mon = list_to_integer([Mon1,Mon2]),
    Day = list_to_integer([Day1,Day2]),
    Hour = list_to_integer([H1,H2]),
    Min = list_to_integer([M1,M2]),
    Sec = list_to_integer([S1,S2]),
    Milli = list_to_integer([T1,T2,T3]),
    TimeStamp = {utc,{Year,Mon,Day},{Hour,Min,Sec},Milli},
    parse_log_data(Data, TimeStamp);
parse_log_2(<<Y1,Y2,Y3,Y4,$-,Mon1,Mon2,$-,Day1,Day2,$-,  %% bug?? T?
	      H1,H2,$:,M1,M2,$:,S1,S2,$.,T1,T2,T3,$,,Data/binary>>) ->
    Year = list_to_integer([Y1,Y2,Y3,Y4]),
    Mon = list_to_integer([Mon1,Mon2]),
    Day = list_to_integer([Day1,Day2]),
    Hour = list_to_integer([H1,H2]),
    Min = list_to_integer([M1,M2]),
    Sec = list_to_integer([S1,S2]),
    Milli = list_to_integer([T1,T2,T3]),
    TimeStamp = {local,{Year,Mon,Day},{Hour,Min,Sec},Milli},
    parse_log_data(Data, TimeStamp);
parse_log_2(<<Y1,Y2,Y3,Y4,$-,Mon1,Mon2,$-,Day1,Day2,$T,
	      H1,H2,$:,M1,M2,$:,S1,S2,$.,T1,T2,T3,$,,Data/binary>>) ->
    Year = list_to_integer([Y1,Y2,Y3,Y4]),
    Mon = list_to_integer([Mon1,Mon2]),
    Day = list_to_integer([Day1,Day2]),
    Hour = list_to_integer([H1,H2]),
    Min = list_to_integer([M1,M2]),
    Sec = list_to_integer([S1,S2]),
    Milli = list_to_integer([T1,T2,T3]),
    TimeStamp = {local,{Year,Mon,Day},{Hour,Min,Sec},Milli},
    parse_log_data(Data, TimeStamp);
parse_log_2(_Other) ->
    io:format("read log_2: ~p\n", [_Other]),
    error.

parse_log_data(Data, _TimeStamp) ->
    case binary:split(Data, <<",">>, [global,trim_all]) of
	[PriBin,PGNBin,SrcBin,DstBin,LenBin | Ds] ->
	    Pri = erlang:binary_to_integer(PriBin),
	    PGN = erlang:binary_to_integer(PGNBin),
	    Src = erlang:binary_to_integer(SrcBin),
	    Dst = erlang:binary_to_integer(DstBin),
	    Len = erlang:binary_to_integer(LenBin),
	    FrameData = list_to_binary(
			  [ erlang:binary_to_integer(D,16) || D <- Ds]),
	    {{Pri,PGN,Src,Dst},Len,FrameData};
	_Other ->
	    io:format("read log_data: ~p\n", [_Other]),
	    error
    end.
