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
%%%                CANopen encode bytes in a little endian fashion.
%%%                Compound data is encoded by first encoding the
%%%                elements and the concatinating the result
%%%
%%% Created : November 2018 by Malotte W Lönne
%%% @end
%%%-------------------------------------------------------------------

-module(nmea_2000_codec).

-export([decode/2]).

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
    decode_compound_e(Data,TypeList).

%% decode parts of data types
decode_e(Data, integer, S) when S=<32-> decode_signed(Data, S);
decode_e(Data, unsigned,S) when S=<32-> decode_unsigned(Data, S);
decode_e(Data, domain, S) -> decode_unsigned(Data, S).

decode_compound_e(Data,[TS = {T,Sz}|TSs]) 
  when Sz =< bit_size(Data) andalso Sz >= 0 -> 
    lager:debug("data ~w, ts ~p, tss ~p", [Data, TS, TSs]), 
    <<Value:Sz/bits, Rest/bits>> = Data,
    lager:debug("value ~w rest ~w", [Value, Rest]), 
    {V,<<>>} = decode_e(Value, T, Sz),
    lager:debug("value ~p", [V]), 
    {Vs,Data1} = decode_compound_e(Rest,TSs),
    lager:debug("values ~p rest ~w", [Vs, Rest]), 
    {[V|Vs], Data1};
decode_compound_e(Data, [_TS = {T,Sz}| _TSs]) 
  when Sz > bit_size(Data) ->
    lager:warning("Not enough bits ~w, ~p", [Data, _TS]),
    {V,<<>>} = decode_e(Data, T, Sz),
    {[V], <<>>};
decode_compound_e(Data, [{_T,Sz}])
  when Sz < 0 ->
    %% Should only happen at end of data 
    {[0], Data};
decode_compound_e(Data, []) ->
    {[], Data}.

decode_signed(Data, Size)   -> 
    <<X:Size/signed-integer-little, Bits/bits>> = Data,
    {X, Bits}.

decode_unsigned(Data, Size) -> 
    <<X:Size/unsigned-integer-little, Bits/bits>> = Data,
    {X, Bits}.

