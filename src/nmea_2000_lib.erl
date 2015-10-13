%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Utils
%%% @end
%%% Created :  9 Sep 2015 by Tony Rogvall <tony@rogvall.se>

-module(nmea_2000_lib).

-export([decode_canid/1, encode_canid/1, encode_canid/4]).
-export([decode_canid2/1]).
-export([convert/0, convert/2]).
-export([load/1, save/2]).
-export([load_pgns_term/0, load_pgns_term/1]).
-export([load_pgns_xml/0, load_pgns_xml/1]).
-export([text_expand/2]).

-include_lib("xmerl/include/xmerl.hrl").

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

convert() ->
    convert(filename:join(code:priv_dir(nmea_2000),"pgns.xml"),
	    filename:join(code:priv_dir(nmea_2000),"pgns.term")).

convert(FromFile, ToFile) ->
    case load(FromFile) of
	{ok, Def} -> save(ToFile, Def);
	Error -> Error
    end.

save(File, Def) ->
    case file:open(File, [write]) of
	{ok,Fd} ->
	    io:format(Fd, "~s\n", ["%% -*- erlang -*-"]),
	    try write_as_term(Fd, Def) of
		ok -> ok
	    catch
		error:Reason ->
		    io:format("crash: ~p\n", 
			      [erlang:get_stacktrace()]),
		    {error,Reason}
	    after
		file:close(Fd)
	    end;
	Error ->
	    Error
    end.

write_as_term(Fd, [{PGN,Info}|Ps]) ->
    io:format(Fd, "~p.\n", [{PGN, Info}]),
    write_as_term(Fd, Ps);
write_as_term(_Fd, []) ->
    ok.

load(File) ->
    case filename:extension(File) of
	".xml" -> 
	    load_pgns_xml(File);
	".term" ->
	    load_pgns_term(File)
    end.

load_pgns_term() ->
    load_pgns_term(filename:join(code:priv_dir(nmea_2000), "pgns.term")).

load_pgns_term(File) ->
    file:consult(File).

load_pgns_xml() ->
    load_pgns_xml(filename:join(code:priv_dir(nmea_2000), "pgns.xml")).

load_pgns_xml(File) ->
    Acc = fun(#xmlText{value = " ", pos = P}, Acc, S) ->
		  {Acc, P, S};  % new return format
	     (X, Acc, S) ->
		  {[X|Acc], S}
	  end,
    SearchPath = code:priv_dir(nmea_2000),
    case xmerl_scan:file(File,
			 [{space,normalize},
			  {validation,off},
			  {acc_fun, Acc},
			  {fetch_path, [SearchPath]}]) of
	Error = {error,_} ->
	    Error;
	{Xml,_Rest} ->
	    Node = xmerl_lib:simplify_element(Xml),
	    Def  = pgn_definitions(Node),
	    %% io:format("Types = ~p\n", [collect_types(Def)]),
	    {ok, Def}
    end.

pgn_definitions({'PGNDefinitions',_As,Cs}) ->
    case lists:keyfind('PGNs', 1, Cs) of
	false -> [];
	{'PGNs',_,PGNs} -> pgns(PGNs, [])
    end.

pgns([{'PGNInfo', _As, Cs} | PGNs], Acc) ->
    pgns(PGNs, [pgn_info(Cs,0,[])|Acc]);
pgns([{_, _As, _Cs} | PGNs], Acc) ->
    pgns(PGNs, Acc);
pgns([], Acc) ->
    lists:reverse(Acc).

pgn_info([{'PGN',_As,[Value]}|T],_PGN,Ps) ->
    pgn_info(T, list_to_integer(Value), Ps);
pgn_info([{'Id',_As,[Value]}|T],PGN,Ps) ->
    pgn_info(T, PGN, [{id,Value}|Ps]);
pgn_info([{'Description',_As,Vs}|T],PGN,Ps) ->
    pgn_info(T, PGN, [{description, string(Vs)}|Ps]);
pgn_info([{'Complete',_As,[Value]}|T],PGN,Ps) ->
    pgn_info(T, PGN, [{complete, boolean(Value)}|Ps]);
pgn_info([{'Length',_As,[Value]}|T],PGN,Ps) ->
    pgn_info(T, PGN, [{length, list_to_integer(Value)}|Ps]);
pgn_info([{'RepeatingFields',_As,[Value]}|T],PGN,Ps) ->
    case list_to_integer(Value) of
	0 -> pgn_info(T, PGN, Ps);
	R -> pgn_info(T, PGN, [{repeating_fields,R}|Ps])
    end;
pgn_info([{'Fields',_As,Cs}|T],PGN,Ps) ->
    pgn_info(T, PGN, [{fields,fields(Cs)}|Ps]);
pgn_info([],PGN,Ps) ->
    {PGN, lists:reverse(Ps)}.

fields([{'Field',_As,Cs} | T]) ->
    [field(Cs, []) | fields(T)];
fields([]) ->
    [].

field([{'Order',_,[Value]}|T], Fs) ->
    field(T, [{order,list_to_integer(Value)}|Fs]);
field([{'Id',_,Value}|T], Fs) ->
    field(T, [{id,string(Value)}|Fs]);
field([{'Name',_,Value}|T], Fs) ->
    field(T, [{name,string(Value)}|Fs]);
field([{'Description',_,Value}|T], Fs) ->
    field(T, [{description,string(Value)}|Fs]);
field([{'BitLength',_,[Value]}|T], Fs) ->
    field(T, [{length,list_to_integer(Value)}|Fs]);
field([{'Match',_,[Value]}|T], Fs) ->
    field(T, [{match,list_to_integer(Value)}|Fs]);
field([{'Type',_,[Value]}|T], Fs) ->
    field(T, [{type,type(Value)}|Fs]);
field([{'Resolution',_,[Value]}|T], Fs) ->
    case list_to_number(Value) of
	0 -> field(T, Fs);
	1 -> field(T, Fs);
	R -> field(T, [{resolution,R}|Fs])
    end;
field([{'Units',_,Vs}|T], Fs) ->
    field(T, [{units,string(Vs)}|Fs]);
field([{'Signed',_,[Value]}|T], Fs) ->
    case boolean(Value) of
	false -> field(T, Fs);
	true  -> field(T, [{signed,true}|Fs])
    end;
field([{'Offset',_,[Value]}|T], Fs) ->
    field(T, [{offset,list_to_integer(Value)}|Fs]);
field([{'EnumValues',_,Pairs}|T], Fs) ->
    field(T, [{enums, pairs(Pairs)}|Fs]);
field([{'BitStart',_,[_Value]}|T], Fs) ->  %% not used
    field(T, Fs);
field([{'BitOffset',_,[_Value]}|T], Fs) -> %% not used
    field(T, Fs);
field([], Fs) ->
    lists:reverse(Fs).

pairs([{'EnumPair',As,_Ps}|T]) ->
    {_,Name} = lists:keyfind('Name',1,As),
    {_,Value} = lists:keyfind('Value', 1, As),
    try list_to_integer(Value) of
	Int -> [{Name,Int} | pairs(T)]
    catch
	error:_ ->
	    [{Name,Value} | pairs(T)]
    end;
pairs([]) ->
    [].

string([]) -> "";
string([Value]) -> Value;
string(Vs) -> lists:flatten(Vs).

boolean("true") -> true;
boolean("false") -> false.

type(undefined) -> undefined;
type("Lookup table") -> enum;
type("Binary data") -> binary;
type("Manufacturer code") -> int;  %% ???
type("Integer") -> int;
type("Temperature") -> int;        %% ??? unit?
type("Angle") -> int;              %% resolution*value = unit?
type("Date") -> int;
type("Time") -> int;
type("Latitude") -> int;
type("Longitude") -> int;
type("Pressure") -> int;
type("IEEE Float") -> float;
type("ASCII text") -> string;                   %% C string
type("String with start/stop byte") -> string;  %% How?
type("Decimal encoded number") -> bcd;
type("ASCII or UNICODE string starting with length and control byte") ->
    string; %% utf8?
type("ASCII string starting with length byte") -> string.


list_to_number(Value) ->
    case string:to_integer(Value) of
	{Int,""} -> Int;
	{Int,E=[$e|_]} -> 
	    list_to_float(integer_to_list(Int)++".0"++E);
	{Int,E=[$E|_]} -> 
	    list_to_float(integer_to_list(Int)++".0"++E);
	{_Int,_F=[$.|_]} ->
	    list_to_float(Value)
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
