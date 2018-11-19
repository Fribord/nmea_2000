%%% coding: latin-1
%%%---- BEGIN COPYRIGHT -----------------------------------------------------
%%% Copyright (C) 2018, Rogvall Invest AB, <tony@rogvall.se>
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
%%%---- END COPYRIGHT ------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @author Malotte W Lönne <malotte@malotte.net>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%% NMEA 2000 bit decoding.
%%%
%%% Created : November 2018 by Malotte W Lönne
%%% @end
%%%-------------------------------------------------------------------

-module(nmea_2000_codec).

-export([decode/2]).

-export([test/0]).

-type type_spec()::
	{Type::atom(),  Size::integer()}.

%% decode/2
%%    decode(Data, Type) -> {Value, BitsTail}
%%
%%--------------------------------------------------------------------
%% @doc
%% Decoded data. 
%%
%% @end
%%--------------------------------------------------------------------
-spec decode(Bin::binary(), list(type_spec())) -> 
		    {Value::term(), BitsTail::binary()}.

decode(Data, TypeList) ->
    lager:debug("data ~p, types ~p", [Data, TypeList]),
    decode(Data,TypeList, 0, []).

decode(Data, [], Pos, Acc) ->
    %% too much data
    Size = bit_size(Data) - Pos,
    <<Value:Size/little>> = extract_lsb_bits(Pos, Size, Data),
    lager:debug("surplus data ~p", [Value]),
    {lists:reverse(Acc), Value};
decode(Data, [{_Type, -1} | _TypeSpecList], Pos, Acc) ->
    %% group rest
    Size = bit_size(Data) - Pos,
    <<Value:Size/little>> = extract_lsb_bits(Pos, Size, Data),
    lager:debug("group rest ~p", [Value]),
    {lists:reverse([Value | Acc]), <<>>};
decode(Data, [{Type, Size} | TypeSpecList], Pos, Acc) 
  when Pos + Size =< bit_size(Data) ->
    case Type of 
	unsigned ->
	    <<Value:Size/unsigned-little>> = extract_lsb_bits(Pos, Size, Data);
	integer ->
	    <<Value:Size/signed-little>> = extract_lsb_bits(Pos, Size, Data);
	float ->
	    <<Value:Size/little-float>> = extract_lsb_bits(Pos, Size, Data);
	domain ->
	    %%??
	    <<Value:Size/unsigned-little>> = extract_lsb_bits(Pos, Size, Data)
    end,
    lager:debug("pos ~p, size ~p, value ~p", [Pos, Size,Value]),    
    decode(Data, TypeSpecList, Pos + Size, [Value | Acc]);
decode(_Data, _TypeSpecList, _Pos, Acc) ->
    lager:warning("not enough data ~w for ~p", [_Data, _TypeSpecList]),
    {lists:reverse(Acc), <<>>}.

%%
%% Edit bit string
%%

%% big endian bit order (pos)
%% example: insert dB dA d9 d8 d7 d6 d5 d4 d3 d2 d1 d0  at position 6
%%
%%  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
%% a0 a1 a2 a3 a4 a5 a6 a7 b0 b1 b2 b3 b4 b5 b6 b7 c0 c1 c2 c3 c4 c5 c6 c7
%%                   dB dA d9 d8 d7 d6 d5 d4 d3 d2 d1 d0
%% 

replace_msb([{Pos,Len,Type,Value}|Spec], Bs) ->
    case Type of
	integer ->
	    Bits = <<Value:Len/signed-big-integer>>,
	    replace_msb(Spec,replace_msb_bits(Pos,Bits,Bs));
	unsigned ->
	    Bits = <<Value:Len/unsigned-big-integer>>,
	    replace_msb(Spec,replace_msb_bits(Pos,Bits,Bs));
	float -> %% len can only be 32 or 64 right now
	    Bits = <<Value:Len/big-float>>,
	    replace_msb(Spec,replace_msb_bits(Pos,Bits,Bs))
    end;
replace_msb([], Bs) ->
    Bs.

%% list of offfset, len and type into bitstring
extract_msb([{Pos,Len,Type}|Spec], Bs) ->
    Value = extract_big_element(Pos,Len,Type,Bs),
    [Value|extract_msb(Spec,Bs)];
extract_msb([], _Bs) ->
    [].

extract_big_element(Pos,Len,Type,Bs) ->
    Bits = extract_msb_bits(Pos,Len,Bs),
    case Type of
	integer ->
	    <<Value:Len/signed-big-integer>> = Bits,
	    Value;
	unsigned ->
	    <<Value:Len/unsigned-big-integer>> = Bits,
	    Value;
	float ->
	    <<Value:Len/big-float>> = Bits,
	    Value
    end.

%% replace Bits at bit position Pos in BitString
replace_msb_bits(Pos, Bits, BitString) when is_bitstring(Bits), 
					    is_bitstring(BitString) ->
    Len = bit_size(Bits),
    BitString1 = ensure_size(BitString, Len+Pos),
    Size = bit_size(Bits),
    <<A:Pos/bits, _:Size/bits, B/bits>> = BitString1,
    <<A/bits, Bits/bits, B/bits>>.

extract_msb_bits(Pos, Len, BitString) ->
    BitString1 = ensure_size(BitString, Len+Pos),
    <<_A:Pos, B:Len/bits, _/bits>> = BitString1,
    B.

%% little endian bit order (pos)
%% example: insert dB dA d9 d8 d7 d6 d5 d4 d3 d2 d1 d0  at position 6
%%
%%  7  6  5  4  3  2  1  0 15 14 13 12 11 10  9  8 23 22 21 20 19 18 17 16
%% a7 a6 a5 a4 a3 a2 a1 a0 b7 b6 b5 b4 b3 b2 b1 b0 c7 c6 c5 c4 c3 c2 c1 c0
%% dB dA                   d9 d8 d7 d6 d5 d4 d3 d2                   d1 d0

replace_lsb([{Pos,Len,Type,Value}|Spec], Bs) ->
    case Type of
	integer ->
	    Bits = <<Value:Len/signed-little-integer>>,
	    replace_lsb(Spec,replace_lsb_bits(Pos,Bits,Bs));
	unsigned ->
	    Bits = <<Value:Len/unsigned-little-integer>>,
	    replace_lsb(Spec,replace_lsb_bits(Pos,Bits,Bs));
	float -> %% len can only be 32 or 64 right now
	    Bits = <<Value:Len/little-float>>,
	    replace_lsb(Spec,replace_lsb_bits(Pos,Bits,Bs))
    end;
replace_lsb([], Bs) ->
    Bs.
    
%% 
%% replace Bits at bit position Pos in BitString
%%

replace_lsb_bits(Pos, Bits, BitString) ->
    Len = bit_size(Bits),
    BitString1 = ensure_size(BitString, ((Len+Pos+7) bsr 3)*8),
    Offs0 = (Pos bsr 3) bsl 3,
    ALen1 = Pos band 7,
    ALen  = (8-ALen1) band 7,
    if Len =< ALen -> %% Data fit in first byte
	    Offs1 = Offs0+(ALen-Len),
	    <<A0:Offs1/bits,_:Len/bits,A2:ALen1/bits,C0/bits>> = BitString1,
	    <<A0/bits, Bits/bits, A2/bits, C0/bits>>;
       true ->
	    TLen = Len - ALen,
	    BLen = (TLen bsr 3)*8, %% middle bits
	    CLen = TLen - BLen,    %% tail
	    CLen1 = (8-CLen) band 7,
	    <<A0:Offs0/bits, _:ALen/bits, A2:ALen1/bits,
	      _:BLen/bits,C1:CLen1/bits,_:CLen/bits,C0/bits>> = BitString1,
	    <<Seg1:ALen/bits, Seg2:BLen/bits, Seg3:CLen/bits>> = Bits,
	    <<A0/bits, Seg1/bits, A2/bits, Seg2/bits, C1/bits, Seg3/bits, C0/bits>>
    end.

%% extract list of length/type elements all given with individual offsets
extract_lsb([{Pos,Len,Type}|Spec], Bs) ->
    Value = extract_little_element(Pos,Len,Type,Bs),
    [Value|extract_lsb(Spec,Bs)];
extract_lsb([], _Bs) ->
    [].

%% extract list of elements given a start offset and length/type
extract_seq_lsb(Spec,Bs) ->
    extract_seq_lsb(0,Spec,Bs).

extract_seq_lsb(Pos,[{Len,Type}|Spec],Bs) ->
    Value = extract_little_element(Pos,Len,Type,Bs),
    [Value|extract_seq_lsb(Pos+Len,Spec,Bs)];
extract_seq_lsb(_Pos,[], _Bs) ->
    [].

extract_little_element(Pos,Len,Type,Bs) ->
    Bits = extract_lsb_bits(Pos,Len,Bs),
    case Type of
	integer ->
	    <<Value:Len/signed-little-integer>> = Bits,
	    Value;
	unsigned ->
	    <<Value:Len/unsigned-little-integer>> = Bits,
	    Value;
	float ->
	    <<Value:Len/little-float>> = Bits,
	    Value
    end.

%% we should maybe expect a binary as input, since lsb bits can not
%% exist without msb bits without loosing information about position
extract_lsb_bits(Pos, Len, BitString) ->
    BitString1 = ensure_size(BitString, Len+Pos),
    Offs0    = (Pos bsr 3) bsl 3,
    ALen1    = Pos band 7,
    ALen     = (8-ALen1) band 7,
    if Len =< ALen ->
	    Offs1 = Offs0 + (ALen-Len),
	    <<_:Offs1/bits,Bits:Len/bits,_/bits>> = BitString1,
	    Bits;
       ALen1 =:= 0 -> %% => ALen = 0
	    BLen = (Len bsr 3)*8,    %% middle bits
	    case Len-BLen of
		0 ->
		    <<_:Offs0/bits,Bits:BLen/bits,_/binary>> = BitString1,
		    Bits;
		CLen ->
		    CLen1 = (8-CLen) band 7,
		    <<_:Offs0/bits,
		      B:BLen/bits,_:CLen1/bits,C2:CLen/bits,_/binary>> = 
			BitString1,
		    <<B/bits,C2/bits>>
	    end;
       true ->
	    TLen = Len - ALen,
	    BLen = (TLen bsr 3)*8,    %% middle bits
	    CLen = TLen - BLen,       %% tail
	    CLen1 = (8-CLen) band 7,
	    <<_:Offs0/bits,A1:ALen/bits,_:ALen1/bits,
	      B:BLen/bits,_:CLen1/bits,C2:CLen/bits,_/binary>> = BitString1,
	    <<A1/bits,B/bits,C2/bits>>
    end.

ensure_size(BitString, NBits) when bit_size(BitString) >= NBits ->
    BitString;
ensure_size(BitString, NBits) ->
    Pad = NBits - bit_size(BitString),
    <<BitString/bits, 0:Pad>>.

display_bits(Name, Bits) when is_bitstring(Bits) ->
    display_bits(Name, Bits,[bit_size(Bits)]).

display_bits(Name, Bits, Gs) when is_bitstring(Bits) ->
    BitString = [I+$0 || <<I:1>> <= Bits],
    io:put_chars([Name,"=","|",format_bit_groups(Gs, BitString),"\n"]).

format_bit_groups([N|Ns], BitString) ->
    {G,BitStringTail} = lists:split(N, BitString),
    [G,"|" | format_bit_groups(Ns, BitStringTail)];
format_bit_groups([], _) ->
    [].

test() ->
    test_pgn_127505(),
    test_pgn_129025(),    
    test_10_5(),
    test_random_lsb(),
    test_random_msb(),
    test_overlap_lsb(),
    test_overlap_msb().

test_pgn_127505() ->
    BitString = <<0,156,49,255,255,255,255,255>>,
    <<Instance:4>>         = extract_lsb_bits(0, 4, BitString),
    <<Type:4>>             = extract_lsb_bits(4, 4, BitString),
    <<Level:16/little>>    = extract_lsb_bits(8, 16, BitString),
    <<Capacity:32/little>> = extract_lsb_bits(24, 32, BitString),
    [0,0,12700,4294967295] = [Instance,Type,Level,Capacity],
    [{instance,Instance},{type,Type},{level,Level},{capacity,Capacity}].

test_pgn_129025() ->
    BitString = <<71,142,87,35,36,129,231,10>>,
    <<Latitude:32/little>>  = extract_lsb_bits(0, 32, BitString),
    <<Longitude:32/little>> = extract_lsb_bits(32, 32, BitString),
    [592940615,182944036] = [Latitude,Longitude],
    [{latitude,Latitude},{longitude,Longitude}].

test_10_5() ->
    Bits1 = <<-423:10/signed-little>>,
    display_bits("-423:10", Bits1),
    BitString1 = replace_lsb_bits(0, Bits1, <<>>),
    display_bits("BitString1", BitString1),
    Bits2 = <<30:5/unsigned-little>>,
    display_bits("30:5", Bits2),
    BitString2 = replace_lsb_bits(10, Bits2, BitString1),
    display_bits("BitString2", BitString2),

    <<16#59,16#7A>> = BitString2,  %% FROM CANopen 301!

    BitsX1 = extract_lsb_bits(0, 10, BitString2),
    display_bits("Bits 0:10", BitsX1),
    <<A:10/signed-little>> = BitsX1,

    BitsX2 = extract_lsb_bits(10, 5, BitString2),
    display_bits("Bits 10:5", BitsX2),
    <<B:5/unsigned-little>> = BitsX2,

    [-423, 30] = [A,B],
    [{a,A},{b,B}].

test_random_lsb() ->
    [(ok=test_random_lsb(Pos, Len)) ||
	Pos <- lists:seq(0, 17),
	Len <- lists:seq(0, 31)],
    ok.

test_overlap_lsb() ->
    [(ok=test_overlap_lsb(Pos,Len1,Gap,Len2)) ||
	Pos <- lists:seq(0, 17),
	Gap <- [0,1],
	Len1 <- lists:seq(0, 12),
	Len2 <- lists:seq(0, 15)].

test_random_msb() ->
    [(ok=test_random_msb(Pos, Len)) ||
	Pos <- lists:seq(0, 17),
	Len <- lists:seq(0, 31)],
    ok.

test_overlap_msb() ->
    [(ok=test_overlap_msb(Pos,Len1,Gap,Len2)) ||
	Pos <- lists:seq(0, 17),
	Gap <- [0,1],
	Len1 <- lists:seq(0, 12),
	Len2 <- lists:seq(0, 15)].

%% move a bit-pattern Value of length Len from position 0 to position N
test_random_lsb(Pos, Len) ->
    RandomBitPattern = crypto:strong_rand_bytes((Pos+Len+7) bsr 3),
    <<Bits:Len/bits,_/bits>> = crypto:strong_rand_bytes((Len+7) bsr 3),
    BitString = replace_lsb_bits(Pos, Bits, RandomBitPattern),
    %% now extract the same bit pattern 
    Bits = extract_lsb_bits(Pos, Len, BitString),
    ok.

%% move a bit-pattern Value of length Len from position 0 to position N
test_random_msb(Pos, Len) ->
    RandomBitPattern = crypto:strong_rand_bytes((Pos+Len+7) bsr 3),
    <<Bits:Len/bits,_/bits>> = crypto:strong_rand_bytes((Len+7) bsr 3),
    BitString = replace_msb_bits(Pos, Bits, RandomBitPattern),
    %% now extract the same bit pattern 
    Bits = extract_msb_bits(Pos, Len, BitString),
    ok.

%% check that there are no overwrite write two areas disjuct
%% (non overlapping)

test_overlap_lsb(Pos, Len1, Gap, Len2) ->
    RandomBitPattern = crypto:strong_rand_bytes((Pos+Len1+Gap+Len2+7) bsr 3),
    <<Bits1:Len1/bits,_/bits>> = crypto:strong_rand_bytes((Len1+7) bsr 3),
    <<Bits2:Len2/bits,_/bits>> = crypto:strong_rand_bytes((Len2+7) bsr 3),
    BitString1 = replace_lsb_bits(Pos, Bits1, RandomBitPattern),
    BitString  = replace_lsb_bits(Pos+Len1+Gap, Bits2, BitString1),

    %% now extract both patterns
    Bits1 = extract_lsb_bits(Pos, Len1, BitString),
    Bits2 = extract_lsb_bits(Pos+Len1+Gap, Len2, BitString),

    ok.

test_overlap_msb(Pos, Len1, Gap, Len2) ->
    RandomBitPattern = crypto:strong_rand_bytes((Pos+Len1+Gap+Len2+7) bsr 3),
    <<Bits1:Len1/bits,_/bits>> = crypto:strong_rand_bytes((Len1+7) bsr 3),
    <<Bits2:Len2/bits,_/bits>> = crypto:strong_rand_bytes((Len2+7) bsr 3),
    BitString1 = replace_msb_bits(Pos, Bits1, RandomBitPattern),
    BitString  = replace_msb_bits(Pos+Len1+Gap, Bits2, BitString1),

    %% now extract both patterns
    Bits1 = extract_msb_bits(Pos, Len1, BitString),
    Bits2 = extract_msb_bits(Pos+Len1+Gap, Len2, BitString),

    ok.
