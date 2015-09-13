%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Generate NMEA Erlang term format
%%% @end
%%% Created :  8 Sep 2015 by Tony Rogvall <tony@rogvall.se>

-module(nmea_2000_gen_pgns).
-export([load/0, load/1]).
-export([save/1, save/0]).
-export([collect_types/1]).

-include_lib("xmerl/include/xmerl.hrl").

-record(field,
	{
	  order :: integer(),
	  id :: string(),
	  name :: string(),
	  description :: string(),
	  length :: integer(),       %% bit length of field
	  type   :: string(),
	  match  :: integer(),
	  resolution :: number(),
	  units  :: string(),
	  signed :: boolean(),
	  offset :: integer(),
	  enums  :: [{string(),integer()}]
	}).

-record(pgn_info,
	{
	  pgn :: integer(),
	  id  :: string(),
	  description :: string(),
	  complete :: boolean(),
	  length :: integer(),    %% total length?
	  repeating_fields :: integer(),
	  fields = [] :: [#field{}]
	}).

save() ->
    save("pgns.term").

save(OutFile) ->
    case file:open(OutFile, [write]) of
	{ok,Fd} ->
	    try load() of
		{ok,Def} ->
		    io:format(Fd, "~s\n", ["%% -*- erlang -*-"]),
		    try write_as_term(Fd, Def) of
			ok -> ok
		    catch
			error:Reason ->
			    io:format("crash: ~p\n", 
				      [erlang:get_stacktrace()]),
			    {error,Reason}
		    end
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

write_as_term(Fd, [P|Ps]) ->
    Info =
	fmt_field(id,P#pgn_info.id,
        fmt_field(complete,P#pgn_info.complete,
	fmt_field(length,P#pgn_info.length,
	fmt_field(repeating_fields,P#pgn_info.repeating_fields,0,
	fmt_field(fields,[format_field(F) || F <- P#pgn_info.fields],[]))))),
    io:format(Fd, "~p.\n", [{P#pgn_info.pgn, Info}]),
    write_as_term(Fd, Ps);
write_as_term(_Fd, []) ->
    ok.

format_field(F) ->
    fmt_field(order,F#field.order,
    fmt_field(id,F#field.id,
    fmt_field(length,F#field.length,
    fmt_field(type,fmt_type(F#field.type),
    fmt_field(units,F#field.units,
    fmt_field(match,F#field.match,
    fmt_field(resolution,F#field.resolution,
    fmt_field(signed,F#field.signed, false,
    fmt_field(offset,F#field.offset,
    fmt_field(enums, F#field.enums,[])))))))))).

fmt_field(_Key,undefined,Acc) -> Acc;
fmt_field(Key,Value,Acc) -> [{Key,Value}|Acc].

fmt_field(_Key,undefined,_Def,Acc) -> Acc;
fmt_field(_Key,Def,Def,Acc) -> Acc;
fmt_field(Key,Value,_Def,Acc) -> [{Key,Value}|Acc].

fmt_type(undefined) -> undefined;
fmt_type("Lookup table") -> enum;
fmt_type("Binary data") -> binary;
fmt_type("Manufacturer code") -> int;  %% ???
fmt_type("Integer") -> int;
fmt_type("Temperature") -> int;        %% ??? unit?
fmt_type("Angle") -> int;              %% resolution*value = unit?
fmt_type("Date") -> int;
fmt_type("Time") -> int;
fmt_type("Latitude") -> int;
fmt_type("Longitude") -> int;
fmt_type("Pressure") -> int;
fmt_type("IEEE Float") -> float;
fmt_type("ASCII text") -> string;                   %% C string
fmt_type("String with start/stop byte") -> string;  %% How?
fmt_type("Decimal encoded number") -> bcd;
fmt_type("ASCII or UNICODE string starting with length and control byte") ->
    string; %% utf8?
fmt_type("ASCII string starting with length byte") -> string.

    
load() ->
    load(filename:join(code:priv_dir(nmea_2000), "pgns.xml")).

load(File) ->
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
    pgns(PGNs, [pgn_info(Cs,#pgn_info{})|Acc]);
pgns([{_, _As, _Cs} | PGNs], Acc) ->
    pgns(PGNs, Acc);
pgns([], Acc) ->
    lists:reverse(Acc).

pgn_info([{'PGN',_As,[Value]}|T],P) ->
    pgn_info(T, P#pgn_info { pgn = list_to_integer(Value)});
pgn_info([{'Id',_As,[Value]}|T],P) ->
    pgn_info(T, P#pgn_info { id = Value });
pgn_info([{'Description',_As,Vs}|T],P) ->
    pgn_info(T, P#pgn_info { description = lists:flatten(Vs) });
pgn_info([{'Complete',_As,[Value]}|T],P) ->
    pgn_info(T, P#pgn_info { complete = boolean(Value) });
pgn_info([{'Length',_As,[Value]}|T],P) ->
    pgn_info(T, P#pgn_info { length = list_to_integer(Value) });
pgn_info([{'RepeatingFields',_As,[Value]}|T],P) ->
    pgn_info(T, P#pgn_info { repeating_fields = list_to_integer(Value) });
pgn_info([{'Fields',_As,Cs}|T],P) ->
    pgn_info(T, P#pgn_info { fields = fields(Cs) });
pgn_info([], P) ->
    P.

fields([{'Field',_As,Cs} | T]) ->
    [field(Cs, #field{}) | fields(T)];
fields([]) ->
    [].

field([{'Order',_,[Value]}|T], F) ->
    field(T, F#field{ order=list_to_integer(Value)});
field([{'Id',_,Value}|T], F) ->
    field(T, F#field{ id=string(Value)});
field([{'Name',_,Value}|T], F) ->
    field(T, F#field{ name=string(Value)});
field([{'Description',_,Value}|T], F) ->
    field(T, F#field{ description=string(Value)});
field([{'BitLength',_,[Value]}|T], F) ->
    field(T, F#field{ length=list_to_integer(Value)});
field([{'Match',_,[Value]}|T], F) ->
    field(T, F#field{ match=list_to_integer(Value)});
field([{'Type',_,[Value]}|T], F) ->
    field(T, F#field{ type=Value });
field([{'Resolution',_,[Value]}|T], F) ->
    field(T, F#field{ resolution=list_to_number(Value)});
field([{'Units',_,Vs}|T], F) ->
    field(T, F#field{ units=lists:flatten(Vs)});
field([{'Signed',_,[Value]}|T], F) ->
    field(T, F#field{ signed=boolean(Value)});
field([{'Offset',_,[Value]}|T], F) ->
    field(T, F#field{ offset=list_to_integer(Value)});
field([{'EnumValues',_,Pairs}|T], F) ->
    field(T, F#field{ enums = pairs(Pairs)});
field([{'BitStart',_,[_Value]}|T], F) ->  %% ignore
    field(T, F);
field([{'BitOffset',_,[_Value]}|T], F) -> %% ignore
    field(T, F);
field([], F) ->
    F.

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
string(Vs) -> list:flatten(Vs).

boolean("true") -> true;
boolean("false") -> false.

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

collect_types(Def) ->
    TypeSet = fold_fields(
		fun(#field { type=T}, Set) -> 
			sets:add_element(T, Set)
		end, sets:new(), Def),
    sets:to_list(TypeSet).


fold_fields(Fun, Acc, [#pgn_info { fields=Fs}|T]) ->
    Acc1 = lists:foldl(Fun, Acc, Fs),
    fold_fields(Fun, Acc1, T);
fold_fields(_Fun, Acc, []) ->
    Acc.
