%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Load Erlang term format and convert to erlang code using
%%%    CANopen encoding ( that is little endian / lsb )
%%% @end
%%% Created :  8 Sep 2015 by Tony Rogvall <tony@rogvall.se>

-module(nmea_2000_gen_co_erl).

-export([save/0]).
-export([convert/2]).
-export([ccm/0, ccm/1, ccm/2]).

save() ->
    convert(filename:join(code:priv_dir(nmea_2000),"pgns.term"),
	    filename:join(code:priv_dir(nmea_2000),"nmea_2000_pgn.erl")).

convert(FromFile, OutFile) ->
    case nmea_2000_lib:load(FromFile) of
	{ok,Def} ->
	    case file:open(OutFile, [write]) of
		{ok,Fd} ->
		    write_header(Fd),
		    try write_functions(Fd, Def) of
			ok -> ok
		    catch
			error:Reason ->
			    io:format("crash: ~p\n", 
				      [erlang:get_stacktrace()]),
			    {error,Reason}
		    after
			file:close(Fd)
		    end
	    end;
	Error ->
	    Error
    end.

%% Converts, compiles and moves.
ccm() ->
    ccm(filename:join(code:priv_dir(nmea_2000),"pgns.term"),
	filename:join(code:priv_dir(nmea_2000),"nmea_2000_pgn.erl")).

ccm(TermFile) ->
    ccm(filename:join(code:priv_dir(nmea_2000),TermFile),
	filename:join(code:priv_dir(nmea_2000),"nmea_2000_pgn.erl")).

ccm(FromFile, OutFile) ->
    ok = convert(FromFile, OutFile),
    file:set_cwd(code:priv_dir(nmea_2000)),
    {ok, _File} = compile:file(OutFile),
    BeamFile = filename:join(code:priv_dir(nmea_2000),
			     filename:basename(OutFile,".erl") ++ ".beam"),
    [] = os:cmd("mv " ++ BeamFile ++ " " ++ code:lib_dir(nmea_2000,ebin)),
    ok.
    

write_header(Fd) ->
    io:format(Fd, "~s\n", ["%% -*- erlang -*-"]),
    io:format(Fd, "~s\n", ["-module(nmea_2000_pgn)."]),
    io:format(Fd, "~s\n", ["-export([is_fast/1])."]),
    io:format(Fd, "~s\n", ["-export([decode/2])."]),
    io:format(Fd, "~s\n", ["-export([decode_/2])."]),
    io:format(Fd, "\n\n", []).

write_functions(Fd, Ps) ->
    Ls = [{PGN,proplists:get_value(length,Fs,8)} || {PGN,Fs} <- Ps],
    Ls1 = lists:usort(Ls),
    Ls2 = group_length(Ls1),
    Ls3 = [{P, lists:max(Ln)} || {P,Ln} <- Ls2],
    write_is_fast(Fd, Ls3, Ps),
    io:format(Fd, "\n\n", []),
    io:format(Fd, 
	      "decode_(Bits, Fields) ->\n"
	      "  try co_codec:decode(Bits, Fields) of\n"
	      "    Result -> Result\n"
	      "  catch\n"
	      "    error:_ -> false\n"
	      "  end.\n\n", []),
    write_decode(Fd, Ps).

write_decode(Fd, PGN,[{PGN,Info}|Ps], Acc) ->
    write_decode(Fd, PGN, Ps, [Info|Acc]);
write_decode(Fd, PGN,[{PGN1,Info}|Ps], Acc) ->
    emit_decode(Fd, PGN, lists:reverse(Acc), ";"),
    write_decode(Fd, PGN1, Ps, [Info]);
write_decode(Fd, PGN, [], Acc) ->
    emit_decode(Fd, PGN, lists:reverse(Acc), ";"),
    io:format(Fd, 
	      "decode(_,<<_/bitstring>>) ->\n"
	      "  false.", []).

emit_decode(Fd, PGN, InfoList, Term) ->
    io:format(Fd, "decode(~p,<<_Bits/bitstring>>) ->\n", [PGN]),
    io:format(Fd, "~s~s\n", [gen_info(InfoList), Term]).

group_length([{P,L}|Ls]) ->
    group_length(Ls, P, [L], []).

group_length([{P,L}|Ps], P, Ls, Acc) ->
    group_length(Ps, P, [L|Ls], Acc);
group_length([{Q,L}|Ps], P, Ls, Acc) ->
    group_length(Ps, Q, [L], [{P,Ls}|Acc]);
group_length([], P, Ls, Acc) ->
    [{P,Ls}|Acc].

%% generate the is_fast function
write_is_fast(Fd, [PGNL], Ps) ->
    emit_is_fast_(Fd, PGNL, ".", Ps);
write_is_fast(Fd, [PGNL|T], Ps) ->
    emit_is_fast_(Fd, PGNL, ";", Ps),
    write_is_fast(Fd, T, Ps).

emit_is_fast_(Fd, {PGN,Length}, Term, Ps) ->
    {PGN,Fs} = lists:keyfind(PGN, 1, Ps),
    Repeating = proplists:get_value(repeating_fields, Fs, 0),
    io:format(Fd, "is_fast(~p) -> ~w~s\n", 
	      [PGN,(Length > 8) orelse (Repeating =/= 0),Term]).
%%
%% generate the decode function
%%
%% Assume PGNs are sorted

write_decode(Fd, [{PGN,Info}|Ps]) ->
    write_decode(Fd, PGN, Ps, [Info]);
write_decode(_Fd, []) ->
    ok.


gen_info([]) ->
    "false";
gen_info([Info|Is]) ->
    Fs = proplists:get_value(fields,Info,[]),
    Repeating = proplists:get_value(repeating_fields,Info,0),
    ByteLength = proplists:get_value(length,Info,0),
    ID = get_id(Info),
    {Fixed,Repeat} = lists:split(length(Fs)-Repeating, Fs),
    F_Fixed  = filter_reserved(Fixed),
    F_Repeat = filter_reserved(Repeat),
    FixedSize = lists:foldl(fun(F,Sum) ->
				    proplists:get_value(length,F)+Sum
			    end, 0, Fixed),
    RepeatSize = lists:foldl(fun(F,Sum) ->
				     proplists:get_value(length,F)+Sum
			     end, 0, Repeat),
    Tail = if RepeatSize > 0 -> false;
	      true -> ByteLength*8 - FixedSize =:= 0
	   end,
    if
	Fixed  =:= [], Repeat =:= [] ->
	    io_lib:format("{~s,[]}", [ID]);
	Fixed =:= [] ->
	    io_lib:format("{~s, lists:append([ begin {[~s],_} = decode_(_RBits, [~s]), [~s] end || <<_RBits:~w/bitstring>> <= _Repeat ])}",
		      [ID,
		       catmap(fun format_match/3, Repeat, false, ","),
		       catmap(fun format_field/3, Repeat, false, ","),
		       catmap(fun format_binding/3,F_Repeat, false,","),
		       RepeatSize
		      ]);
	Repeat =:= [] ->
	    io_lib:format("case decode_(_Bits, [~s]) of\n"
			  "  {[~s],_} -> {~s,[~s]};\n"
			  "  _ -> ~s\n"
			  "end",
			  [
			   catmap(fun format_field/3, Fixed, Tail, ","),
			   catmap(fun format_match/3, Fixed, false, ","),
			   ID,
			   catmap(fun format_binding/3, F_Fixed, false, ","),
			   gen_info(Is)
			  ]);
	true ->
	    io_lib:format("case decode_(_Bits, [~s]) of\n"
			  "  {[~s],_Repeat} ->\n"
			  "    {~s,[~s]++lists:append([ begin {[~s],_} = decode_(_RBits, [~s]), [~s] end || <<_RBits:~w/bitstring>> <= _Repeat ])};\n"
			  "  _ -> ~s\n"
			  "end",
		      [catmap(fun format_field/3, Fixed, false, ","),
		       catmap(fun format_match/3, Fixed, false, ","),
		       ID,
		       catmap(fun format_binding/3,F_Fixed, false, ","),

		       catmap(fun format_match/3, Repeat, false, ","),
		       catmap(fun format_field/3, Repeat, false, ","),
		       catmap(fun format_binding/3,F_Repeat, false,","),
		       RepeatSize,
		       gen_info(Is)
		      ])
    end.


catmap(_Fun, [], _Arg, _Sep) ->
    [];
catmap(Fun, [F], Arg, _Sep) ->
    [Fun(F,true,Arg)];
catmap(Fun, [F|Fs], Arg, Sep) ->
    [Fun(F,false,Arg),Sep | catmap(Fun, Fs, Arg, Sep)].

format_match(F,_Last,_AllowLast) ->
    [case proplists:get_value(match,F) of
	 undefined -> get_var(F);
	 Match -> integer_to_list(Match)
     end].

format_field(F,Last,Tail) ->
    Signed = proplists:get_value(signed, F, false),
    Type   = proplists:get_value(type, F, int),
    BitLen = proplists:get_value(length, F),
    Size   = if Last, Type =:= binary, Tail ->
		     -1;
		true ->
		     BitLen
	     end,
    BitType = case Type of
		  int when Size =< 32 -> if Signed -> "integer";
					    true -> "unsigned"
					 end;
		  int when Size =< 64 -> if Signed -> "integer64";
					    true -> "unsigned64"
					 end;
		  float when Size =:= 32 -> "real32";
		  float when Size =:= 64 -> "real64";
		  binary -> "domain";
		  enum -> "unsigned";
		  string -> "domain"; %% interpret later
		  bcd -> "domain"     %% interpret later
	      end,
    ["{",BitType,",",integer_to_list(Size),"}"].


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
	    Res = proplists:get_value(resolution,F),
	    if Res =:= undefined; Res =:= 0; Res =:= 1 ->
		    ["{",ID,",",Var,"}"];
	       true ->
		    Const = format_number(Res),
		    ["{",ID,",",Var,"*",Const,"}"]
	    end;
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

format_number(R) when is_integer(R) ->
    integer_to_list(R);
format_number(R) when is_float(R) ->
    io_lib_format:fwrite_g(R).




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
