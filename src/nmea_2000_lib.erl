%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Utils
%%% @end
%%% Created :  9 Sep 2015 by Tony Rogvall <tony@rogvall.se>

-module(nmea_2000_lib).

-export([decode_canid/1, encode_canid/1, encode_canid/4]).
-export([decode_canid2/1]).

%% decode a 29-bit canID
decode_canid(CanID) ->
    <<Pri:3,_:1,DP:1,IDA:8,IDB:8,Src:8>> = <<CanID:29>>,
    if IDA < 240 ->
	    Dst = IDB,
	    PGN = (DP bsl 16) + (IDA bsl 8),
	    {Pri,PGN,Src,Dst};
       true ->
	    PGN = (DP bsl 16) + (IDA bsl 8) + IDB,
	    {Pri,PGN,Src,16#ff}
    end.

%% Nicer version (test it!)
decode_canid2(CanID) ->
    case <<CanID:29>> of
	<<Pri:3,_:1,DP:1,IDA:8,Dst:8,Src:8>> when IDA < 240 ->
	    PGN = (DP bsl 16) + (IDA bsl 8),
	    {Pri,PGN,Src,Dst};
	<<Pri:3,_:1,PGN:17,Src:8>> ->
	    {Pri,PGN,Src,16#ff}
    end.

encode_canid({Pri,PGN,Src,Dst}) ->
    encode_canid(Pri,PGN,Src,Dst).

encode_canid(Pri,PGN,Src,16#ff) ->
    <<ID:29>> = <<Pri:3,0:1,PGN:17,Src:8>>,
    ID;
encode_canid(Pri,PGN,Src,Dst) ->
    <<ID:29>> = <<Pri:3,0:1,(PGN bsr 8):9,Dst:8,Src:8>>,
    ID.
