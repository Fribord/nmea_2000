%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Load Erlang term format and convert to erlang code
%%% @end
%%% Created :  8 Sep 2015 by Tony Rogvall <tony@rogvall.se>

-module(nmea_2000_gen_erl).

-export([save/1, save/0]).

save() ->
    save("nmea_2000_pgn.erl").

save(OutFile) ->
    case file:open(OutFile, [write]) of
	{ok,Fd} ->
	    try load() of
		{ok,Def} ->
		    write_header(Fd),
		    try write_functions(Fd, Def) of
			ok -> ok
		    catch
			error:Reason ->
			    io:format("crash: ~p\n", 
				      [erlang:get_stacktrace()]),
			    {error,Reason}
		    end
	    catch 
		error:Reason ->
		    {error,Reason}
	    after
		file:close(Fd)
	    end;
	Error ->
	    Error
    end.
		
load() ->
    load("pgns.term").

load(File) ->
    file:consult(filename:join(code:priv_dir(nmea_2000), File)).

write_header(Fd) ->
    io:format(Fd, "~s\n", ["%% -*- erlang -*-"]),
    io:format(Fd, "~s\n", ["-module(nmea_2000_pgn)."]),
    io:format(Fd, "~s\n", ["-export([is_small/1])."]),
    io:format(Fd, "~s\n", ["-export([decode/2])."]),
    io:format(Fd, "\n\n", []).

write_functions(Fd, Ps) ->
    Ls = [{PGN,proplists:get_value(length,Fs,8)} || {PGN,Fs} <- Ps],
    Ls1 = lists:usort(Ls),
    Ls2 = group_length(Ls1),
    Ls3 = [{P, lists:max(Ln)} || {P,Ln} <- Ls2],
    write_is_small(Fd, Ls3, Ps),
    io:format(Fd, "\n\n", []),
    write_decode(Fd, Ps).

group_length([{P,L}|Ls]) ->
    group_length(Ls, P, [L], []).

group_length([{P,L}|Ps], P, Ls, Acc) ->
    group_length(Ps, P, [L|Ls], Acc);
group_length([{Q,L}|Ps], P, Ls, Acc) ->
    group_length(Ps, Q, [L], [{P,Ls}|Acc]);
group_length([], P, Ls, Acc) ->
    [{P,Ls}|Acc].

%% generate the is_small function
write_is_small(Fd, [PGNL], Ps) ->
    emit_is_small_(Fd, PGNL, ".", Ps);
write_is_small(Fd, [PGNL|T], Ps) ->
    emit_is_small_(Fd, PGNL, ";", Ps),
    write_is_small(Fd, T, Ps).

emit_is_small_(Fd, {PGN,Length}, Term, Ps) ->
    {PGN,Fs} = lists:keyfind(PGN, 1, Ps),
    Repeating = proplists:get_value(repeating_fields, Fs, 0),
    io:format(Fd, "is_small(~p) -> ~w~s\n", 
	      [PGN,(Length =< 8) andalso (Repeating =:= 0),Term]).

%% generate the decode function
%% FIXME! repeating field need extra function to parse tail!
write_decode(Fd, [{PGN,Info}]) ->
    write_decode(Fd, PGN, Info, ".");
write_decode(Fd, [{PGN,Info}|Ps]) ->
    write_decode(Fd, PGN, Info, ";"),
    write_decode(Fd, Ps).

write_decode(Fd, PGN, Info, Term) ->
    Fs = proplists:get_value(fields,Info,[]),
    Repeating = proplists:get_value(repeating_fields,Info,0),
    ID = get_id(Info),
    {Fixed,Repeat} = lists:split(length(Fs)-Repeating, Fs),
    if
	Fixed  =:= [], Repeat =:= [] ->
	    io:format(Fd, "decode(~p,<<_/bitstring>>) ->\n  {~s,[]}~s\n",
		      [PGN,
		       ID,
		       Term]);
	Fixed =:= [] ->
	    io:format(Fd, "decode(~p,<<_Repeat/bitstring>>) ->\n  {~s, "
		      "lists:append([ [~s] || <<~s>> <= _Repeat])}~s\n",
		      [PGN,
		       ID,
		       catmap(fun format_binding/3,filter_reserved(Repeat),
			      false, ","),
		       catmap(fun format_field/3, Repeat, 
			      false, ","),
		       Term]);
	Repeat =:= [] ->
	    io:format(Fd, "decode(~p,<<~s>>) ->\n  {~s,[~s]}~s\n",
		      [PGN,
		       catmap(fun format_field/3, Fixed, true, ","),
		       ID,
		       catmap(fun format_binding/3, filter_reserved(Fixed), 
			      false, ","),
		       Term]);
       true ->
	    io:format(Fd, "decode(~p,<<~s,_Repeat/bitstring>>) ->\n  {~s,[~s | "
		      "lists:append([ [~s] || <<~s>> <= _Repeat])]}~s\n",
		      [PGN,
		       catmap(fun format_field/3, Fixed, false, ","),
		       ID,
		       catmap(fun format_binding/3,filter_reserved(Fixed),
			      false,","),
		       catmap(fun format_binding/3,filter_reserved(Repeat),
			      false,","),
		       catmap(fun format_field/3, Repeat, 
			      false, ","),
		       Term])
    end.


catmap(_Fun, [], _Arg, _Sep) ->
    [];
catmap(Fun, [F], Arg, _Sep) ->
    [Fun(F,true,Arg)];
catmap(Fun, [F|Fs], Arg, Sep) ->
    [Fun(F,false,Arg),Sep | catmap(Fun, Fs, Arg, Sep)].

format_field(F,Last,AllowLast) ->
    Var = case proplists:get_value(match,F) of
	      undefined -> get_var(F);
	      Match -> integer_to_list(Match)
	  end,
    Size = proplists:get_value(length, F),
    Signed = proplists:get_value(signed, F, false),
    Type   = proplists:get_value(type, F, int),
    BitType = case Type of
		  int -> if Signed -> "little-signed";
			    true -> "little-unsigned"
			 end;
		  float -> "little-float";
		  binary -> "bitstring";
		  enum -> "little-unsigned";
		  string -> "bitstring"; %% interpret later
		  bcd -> "bitstring"     %% interpret later
	      end,
    if Type =:= binary, Last, AllowLast ->  %% ends with dynamic binary!
	    [Var,"/",BitType];
       Last, AllowLast ->
	    [Var,":",integer_to_list(Size),"/",BitType,",",
	     "_/bitstring"];
       true ->
	    [Var,":",integer_to_list(Size),"/",BitType]
    end.

%% filter reserved field not wanted in result list
filter_reserved(Fs) ->
    lists:foldr(fun(F,Acc) ->
			case proplists:get_value(id,F) of
			    "reserved" -> Acc;
			    "" -> Acc;
			    _ -> [F|Acc]
			end
		end, [], Fs).

format_binding(F,_Last,_AllowLast) ->
    case proplists:get_value(match,F) of
	undefined ->
	    ID = get_id(F),
	    Var = get_var(F),
	    ["{",ID,",",Var,"}"];
	Match ->
	    ID = get_id(F),
	    ["{",ID,",",integer_to_list(Match),"}"]
    end.

%% works for both pgn info and fields
get_id(F) ->
    case proplists:get_value(id, F) of
	Cs0=[C|_] when C >= $a, C =< $z -> 
	    maybe_quote(Cs0);
	[C|Cs] when C >= $A, C =< $Z -> 
	    maybe_quote([string:to_lower(C)|Cs]);
	Cs when is_list(Cs) -> 
	    "'" ++ Cs ++ "'";
	undefined -> "unknown"
    end.

maybe_quote(String) ->
    case reserved_word(String) of
	true -> "'"++String++"'";
	false -> String
    end.

get_var(F) ->
    case proplists:get_value(id, F) of
	"" ->
	    Order = proplists:get_value(order, F),
	    "_"++integer_to_list(Order);
	"reserved" ->
	    "_";
	ID ->
	    varname(ID)
    end.

varname(Cs0=[C|Cs]) ->
    if C >= $a, C =< $z -> [string:to_upper(C)|Cs];
       C >= $A, C =< $Z -> Cs0;
       true -> [$_|Cs0]
    end.

reserved_word("after") -> true;
reserved_word("begin") -> true;
reserved_word("case") -> true;
reserved_word("try") -> true;
reserved_word("cond") -> true;
reserved_word("catch") -> true;
reserved_word("andalso") -> true;
reserved_word("orelse") -> true;
reserved_word("end") -> true;
reserved_word("fun") -> true;
reserved_word("if") -> true;
reserved_word("let") -> true;
reserved_word("of") -> true;
reserved_word("receive") -> true;
reserved_word("when") -> true;
reserved_word("bnot") -> true;
reserved_word("not") -> true;
reserved_word("div") -> true;
reserved_word("rem") -> true;
reserved_word("band") -> true;
reserved_word("and") -> true;
reserved_word("bor") -> true;
reserved_word("bxor") -> true;
reserved_word("bsl") -> true;
reserved_word("bsr") -> true;
reserved_word("or") -> true;
reserved_word("xor") -> true;
reserved_word(_) -> false.
