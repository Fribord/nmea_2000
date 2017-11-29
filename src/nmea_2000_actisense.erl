%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2016, Rogvall Invest AB, <tony@rogvall.se>
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
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @author Marina Westman Lonne <malotte@malotte.net>
%%% @copyright (C) 2016, Tony Rogvall
%%% @doc
%%%    Actisense NGT-1 NMEA 2000 interface
%%% @end
%%% Created : 16 Sep 2015 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(nmea_2000_actisense).

-behaviour(gen_server).

-include("../include/nmea_2000.hrl").

%% API
-export([start/0, start/1, start/2]).
-export([start_link/0, start_link/1, start_link/2]).
-export([stop/1]).

%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2,
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

%% Test API
-export([pause/1, resume/1, ifstatus/1]).
-export([dump/1]).

%% export for testing
-export([escape/1, unescape/1]).
-export([encode_message/2, decode_message/1]).
-export([find_message/1]).

-record(s, {
	  name::string(),
	  receiver={nmea_2000_router, undefined, 0} ::
	    {Module::atom(), %% Module to join and send to
	     Pid::pid() | undefined,     %% Pid if not default server
	     If::integer()}, %% Interface id
	  uart,            %% serial line port id
	  device,          %% device name
	  baud_rate,       %% baud rate to actisense
	  offset,          %% Usb port offset
	  retry_interval,  %% Timeout for open retry
	  retry_timer,     %% Timer reference for retry
	  pause = false,   %% Pause input
	  alarm = false ::boolean(),
	  buf = <<>>,      %% parse buffer
	  fs               %% can_filter:new()
	 }).

-type nmea_2000_actisense_option() ::
	{device,  DeviceName::string()} |
	{name,    IfName::string()} |
	{baud,    DeviceBaud::integer()} |
	{retry_interval, ReopenTimeout::timeout()} |
	{pause, Pause::boolean()}.

-define(SUBSYS, ?MODULE).
-define(SERVER, ?MODULE).
-define(ALARM_DOWN, 'interface-down').
-define(ALARM_ERROR, 'interface-error').


-define(DEFAULT_RETRY_INTERVAL,  2000).
-define(DEFAULT_BAUDRATE,        115200).
-define(DEFAULT_IF,              0).

-define(COMMAND_TIMEOUT, 500).

-define(MAX_MESSAGE_LEN, 512).  %% max unescaped message

-define(NGT_STARTUP_SEQ, <<16#11, 16#02, 16#00>>).

-define(STX, 16#02).  %% Start packet
-define(ETX, 16#03).  %% End packet
-define(DLE, 16#10).  %% Start pto encode a STX or ETX send DLE+STX or DLE+ETX

-define(N2K_MSG_RECEIVED, 16#93). %% Receive standard N2K message
-define(N2K_MSG_SEND,     16#94). %% Send N2K message
-define(NGT_MSG_RECEIVED, 16#A0). %% Receive NGT specific message
-define(NGT_MSG_SEND,     16#A1). %% Send NGT message

%%%===================================================================
%%% API
%%%===================================================================
-spec start() -> {ok,pid()} | {error,Reason::term()}.
start() ->
    start(1,[]).

-spec start(BudId::integer()) -> {ok,pid()} | {error,Reason::term()}.
start(BusId) ->
    start(BusId,[]).

-spec start(BudId::integer(),Opts::[nmea_2000_actisense_option()]) ->
		   {ok,pid()} | {error,Reason::term()}.
start(BusId, Opts) ->
    nmea_2000:start(),
    ChildSpec= {{?MODULE,BusId}, {?MODULE, start_link, [BusId,Opts]},
		permanent, 5000, worker, [?MODULE]},
    supervisor:start_child(nmea_2000_if_sup, ChildSpec).

-spec start_link() -> {ok,pid()} | {error,Reason::term()}.
start_link() ->
    start_link(1,[]).

-spec start_link(BudId::integer()) -> {ok,pid()} | {error,Reason::term()}.
start_link(BusId) when is_integer(BusId) ->
    start_link(BusId,[]).

-spec start_link(BusId::integer(),Opts::[nmea_2000_actisense_option()]) ->
			{ok,pid()} | {error,Reason::term()}.
start_link(BusId, Opts) when is_integer(BusId), is_list(Opts) ->
    gen_server:start_link(?MODULE, [BusId,Opts], []).

-spec stop(BusId::integer()) -> ok | {error,Reason::term()}.

stop(BusId) ->
    case supervisor:terminate_child(nmea_2000_if_sup, {?MODULE, BusId}) of
	ok ->
	    supervisor:delete_child(nmea_2000_sup, {?MODULE, BusId});
	Error ->
	    Error
    end.

-spec pause(Id::integer() | pid() | string()) -> ok | {error, Error::atom()}.
pause(Id) when is_integer(Id); is_pid(Id); is_list(Id) ->
    call(Id, pause).
-spec resume(Id::integer() | pid() | string()) -> ok | {error, Error::atom()}.
resume(Id) when is_integer(Id); is_pid(Id); is_list(Id) ->
    call(Id, resume).
-spec ifstatus(If::integer() | pid() | string()) ->
		      {ok, Status::atom()} | {error, Reason::term()}.
ifstatus(Id) when is_integer(Id); is_pid(Id); is_list(Id) ->
    call(Id, ifstatus).

-spec dump(Id::integer()| pid() | string()) -> ok | {error, Error::atom()}.
dump(Id) when is_integer(Id); is_pid(Id); is_list(Id) ->
    call(Id,dump).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Id,Opts]) ->
    Device = case proplists:get_value(device, Opts) of
		 undefined ->
		     %% try environment
		     os:getenv("ACTISENSE_DEVICE_" ++ integer_to_list(Id));
		 D -> D
	     end,
    if Device =:= false; Device =:= "" ->
	    lager:error("missing device argument"),
	    {stop, einval};
       true ->
	    Name = proplists:get_value(name, Opts,
				       atom_to_list(?MODULE) ++ "-" ++
					   integer_to_list(Id)),
	    Router = proplists:get_value(router, Opts, nmea_2000_router),
	    Pid = proplists:get_value(receiver, Opts, undefined),
	    RetryInterval = proplists:get_value(retry_interval,Opts,
						?DEFAULT_RETRY_INTERVAL),
	    Pause = proplists:get_value(pause, Opts, false),
	    Baud = case proplists:get_value(baud, Opts) of
		       undefined ->
			   %% maybe ACTISENSE_SPEED_<x>
			   case os:getenv("ACTISENSE_SPEED") of
			       false -> ?DEFAULT_BAUDRATE;
			       ""    -> ?DEFAULT_BAUDRATE;
			       Baud0 -> list_to_integer(Baud0)
			   end;
		       Baud1 -> Baud1
		   end,
	    case join(Router, Pid, {?MODULE,Device,Id, Name}) of
		{ok, If} when is_integer(If) ->
		    lager:debug("joined: intf=~w", [If]),
		    S = #s{ name = Name,
			    receiver={Router,Pid,If},
			    device = Device,
			    offset = Id,
			    baud_rate = Baud,
			    retry_interval = RetryInterval,
			    pause = Pause,
			    fs=nmea_2000_filter:new()
			  },
		    lager:info("using device ~s@~w\n", [Device, Baud]),
		    case open(S) of
			{ok, S1} -> {ok, S1};
			{Error, _S1} -> {stop, Error}
		    end;
		{error, Reason} = E ->
		    lager:error("Failed to join ~p(~p), reason ~p", 
				[Router, Pid, Reason]),
		    {stop, E}
	    end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({send,Packet}, _From, S=#s {uart = Uart})  
  when Uart =/= undefined ->
    {Reply,S1} = send_n2k_message(Packet,S),
    {reply, Reply, S1};
handle_call({send,Packet}, _From, S) ->
    lager:warning("Packet ~p dropped", [Packet]),
    {reply, ok, S};
handle_call(statistics,_From,S) ->
    {reply,{ok,nmea_2000_counter:list()}, S};
handle_call(pause, _From, S=#s {pause = false, uart = Uart}) ->
    lager:debug("pause.", []),
    S1 = if Uart =/= undefined ->
		lager:debug("closing device ~s", [S#s.device]),
		R = uart:close(S#s.uart),
		lager:debug("closed ~p", [R]),
		S#s {uart = undefined};
	   true ->
		S
	end,
    elarm:clear(?ALARM_DOWN, ?SUBSYS),
    elarm:clear(?ALARM_ERROR, ?SUBSYS),
    {reply, ok, S1#s {pause = true, alarm = false}};
handle_call(pause, _From, S=#s {pause = true}) ->
    lager:debug("pause when not active.", []),
    {reply, ok, S};
handle_call(resume, _From, S=#s {pause = true}) ->
    lager:debug("resume.", []),
    case open(S#s {pause = false}) of
	{ok, S1} -> {reply, ok, S1};
	{Error, S1} -> {reply, Error, S1}
    end;
handle_call(resume, _From, S=#s {pause = false}) ->
    lager:debug("resume when not paused.", []),
    {reply, ok, S};
handle_call(ifstatus, _From, S=#s {pause = true}) ->
    lager:debug("ifstatus.", []),
    {reply, {ok, paused}, S};
handle_call(ifstatus, _From, S=#s {pause = false, uart = undefined}) ->
    lager:debug("ifstatus.", []),
    {reply, {ok, faulty}, S};
handle_call(ifstatus, _From, S) ->
    lager:debug("ifstatus.", []),
    {reply, {ok, active}, S};
handle_call(dump, _From, S) ->
    lager:debug("dump.", []),
    {reply, {ok, S}, S};
handle_call(stop, _From, S) ->
    {stop, normal, ok, S};
handle_call(_Request, _From, S) ->
    lager:debug("unknown request ~p\n", [_Request]),
    {reply, {error,bad_call}, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({send,Packet}, S) ->
    {_, S1} = send_n2k_message(Packet, S),
    {noreply, S1};
handle_cast({statistics,From},S) ->
    gen_server:reply(From, {ok,nmea_2000_counter:list()}),
    {noreply, S};
handle_cast({add_filter,From,Accept,Reject}, S) ->
    Fs = nmea_2000_filter:add(Accept,Reject,S#s.fs),
    gen_server:reply(From, ok),
    {noreply, S#s { fs=Fs }};
handle_cast({del_filter,From,Accept,Reject}, S) ->
    Fs = nmea_2000_filter:del(Accept,Reject,S#s.fs),
    gen_server:reply(From, ok),
    {noreply, S#s { fs=Fs }};
handle_cast({default_filter,From,Default}, S) ->
    Fs = nmea_2000_filter:default(Default,S#s.fs),
    gen_server:reply(From, ok),
    {noreply, S#s { fs=Fs }};
handle_cast({get_filter,From}, S) ->
    Reply = nmea_2000_filter:get(S#s.fs),
    gen_server:reply(From, Reply),
    {noreply, S};
handle_cast(_Mesg, S) ->
    lager:debug("unknown message ~p\n", [_Mesg]),
    {noreply, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({uart,U,Data}, S) when S#s.uart =:= U ->
    S1 = receive_data(Data, S),
    {noreply, S1};
handle_info({uart_error,U,Reason},
	    S=#s{uart = U, name = Name, device = DeviceName}) ->
    if Reason =:= enxio ->
	    raise_alarm(?ALARM_DOWN, Name, DeviceName, enxio, 
			[{warning, "maybe unplugged?"}]);
       true ->
	    raise_alarm(?ALARM_ERROR, Name, DeviceName, Reason, []),
	    {noreply, S#s {alarm = true}}
    end;

handle_info({uart_closed,U}, 
	    S=#s{uart = U, name = Name, device = DeviceName}) ->
    raise_alarm(?ALARM_DOWN, Name, DeviceName, uart_closed, []),
    S1 = reopen(S#s {alarm = true}),
    {noreply, S1};

handle_info({timeout,TRef,reopen},S) when TRef =:= S#s.retry_timer ->
    case open(S#s { retry_timer = undefined }) of
	{ok, S1} ->
	    {noreply, S1};
	{Error, S1} ->
	    {stop, Error, S1}
    end;

handle_info(_Info, S) ->
    lager:debug("unknown info ~p", [_Info]),
    {noreply, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

open(S=#s {pause = true}) ->
    {ok, S};
open(S0=#s {name = Name, device = DeviceName, baud_rate = Baud }) ->
    UartOpts = [{mode,binary}, {baud, Baud}, {packet,0},
		{csize, 8}, {stopb,1}, {parity,none}, {active, true}],
    case uart:open1(DeviceName, UartOpts) of
	{ok,Uart} ->
	    lager:debug("~s@~w", [DeviceName,Baud]),
	    elarm:clear(?ALARM_DOWN, ?SUBSYS),
	    elarm:clear(?ALARM_ERROR, ?SUBSYS),
	    send_message(Uart, ?NGT_MSG_SEND, ?NGT_STARTUP_SEQ),
	    %% fixme wait 2 secs ????
	    {ok, S0#s { uart = Uart, alarm = false }};
	{error,E} when E =:= eaccess; E =:= enoent ->
	    lager:debug("~s@~w  error ~w, will try again in ~p msecs.", 
			[DeviceName,Baud,E,S0#s.retry_interval]),
	    raise_alarm(?ALARM_DOWN, Name, DeviceName, E, 
			if E =:= enoent -> [{warning, "Maybe unplugged?"}];
			   true -> []
			end),
	    Timer = start_timer(S0#s.retry_interval, reopen),
	    {ok, S0#s { retry_timer = Timer }};
	{error, E} ->
	    raise_alarm(?ALARM_DOWN, Name, DeviceName, E, []),
	    {E, S0#s {alarm = true}}
    end.

reopen(S=#s {pause = true}) ->
    S;
reopen(S=#s {device = DeviceName}) ->
    if S#s.uart =/= undefined ->
	    lager:debug("closing device ~s", [DeviceName]),
	    R = uart:close(S#s.uart),
	    lager:debug("closed ~p", [R]),
	    R;
       true ->
	    ok
    end,
    Timer = start_timer(S#s.retry_interval, reopen),
    S#s { uart=undefined, buf=(<<>>), retry_timer=Timer }.

start_timer(undefined, _Tag) ->
    undefined;
start_timer(infinity, _Tag) ->
    undefined;
start_timer(Time, Tag) ->
    erlang:start_timer(Time,self(),Tag).

join(Module, Pid, Arg) when is_atom(Module), is_pid(Pid) ->
    Module:join(Pid, Arg);
join(undefined, Pid, _Arg) when is_pid(Pid) ->
    %% No join
    ?DEFAULT_IF;
join(Module, undefined, Arg) when is_atom(Module) ->
    Module:join(Arg).

send_n2k_message(Data, S) ->
    try iolist_to_binary(Data) of
	Data1 ->
	    Reply = send_message(S#s.uart, ?N2K_MSG_SEND, Data1),
	    {Reply, S}
    catch
	error:Reason ->
	    {{error,Reason}, S}
    end.
    
%%
%% message DLE STX <command> <len> [<data> ...]  <checksum> DLE ETX
%% DLE is escaped in that as DLE DLE
%% sum ( command len, unescaped data, checksum ) = 0 mod 256
send_message(Uart, Command, Data) when is_binary(Data) ->
    uart:send(Uart, encode_message(Command, Data)).

receive_data(Data, S) when is_binary(Data) ->
    process_message(<<(S#s.buf)/binary, Data/binary>>, S).

%% throw all data not matching DLE STX

process_message(Buf, S) ->
    case find_message(Buf) of
	{{ok,{Command,Message}},Buf1} ->
	    S1 = input_msg(Command,Message,S),
	    process_message(Buf1, S1);
	{more, Buf1} ->
	    S#s { buf = Buf1 };
	{{error,bad_checksum},Buf1} ->
	    lager:debug("scan_dle_stx, bad message checksum", []),
	    process_message(Buf1, S);
	{{error,message_too_short},Buf1} ->
	    lager:warning("scan_dle_stx, message too short", []),
	    process_message(Buf1, S);
	{{error,message_too_long},Buf1} ->
	    lager:warning("scan_dle_stx, message too long", []),
	    process_message(Buf1, S);
	{{error,empty_message},Buf1} ->
	    lager:warning("scan_dle_stx, empty message", []),
	    process_message(Buf1, S)
    end.

find_message(Buf1 = <<?DLE,?STX,Buf2/binary>>) -> 
    case find_end(Buf2, 0, ?MAX_MESSAGE_LEN) of
	false ->
	    {more, Buf1};
	error ->
	    {{error,message_too_long}, Buf2};
	0 ->
	    <<?DLE,?ETX,Buf3/binary>> = Buf2, %% skip
	    {{error,empty_message}, Buf3};
	I ->
	    Sz = I+4,  %% include ?DLE,STX ... ?DLE,?ETX
	    <<Data2:Sz/binary,Buf3/binary>> = Buf1,
	    Res = decode_message(Data2),
	    {Res, Buf3}
    end;
find_message(Data = <<?DLE>>) -> {more, Data};
find_message(<<?DLE,?DLE,Data/binary>>) -> find_message(Data);
find_message(<<_,Data/binary>>) -> find_message(Data);
find_message(<<>>) -> {more, <<>>}.

%% locate DLX ETX pair
find_end(<<?DLE,?ETX,_/binary>>, I, _Max) -> I;
find_end(<<?DLE,?DLE,Rest/binary>>, I, Max) -> find_end(Rest, I+2, Max);
find_end(<<_,_Rest/binary>>, I, Max) when I>=Max -> error;
find_end(<<_,Rest/binary>>, I, Max) -> find_end(Rest, I+1, Max);
find_end(<<>>, _I, _Max) -> false.

input_msg(?N2K_MSG_RECEIVED, <<Prio,PGN:24/little,Dst,Src,
			       TimeStamp:4/binary,  %% is order here?
			       Len, Data/binary>>, S) ->
    if Len > 223 ->
	    lager:warning("n2k message length too long ~w", [Len]),
	    S;
       true ->
	    Packet = #nmea_packet { pgn = PGN,
				    order = 0,
				    index = 0,
				    prio  = Prio,
				    src   = Src,
				    dst   = Dst,
				    len   = Len,
				    totlen = Len,
				    data = Data },
	    lager:debug("prio:~w,pgn:~w,src:~w,dst:~w,ts=~w,len=~w,data=~p",
		   [Prio,PGN,Src,Dst,TimeStamp,Len,Data]),
	    input(Packet, S)
    end;
input_msg(?NGT_MSG_RECEIVED, _Data, S) ->
    %% io:format("ngt-message: ~p\n", [_Data]),
    S.

input(Packet, S=#s {receiver = Receiver, fs = Fs}) ->
    case nmea_2000_filter:input(Packet, Fs) of
	true ->
	    input_packet(Packet, Receiver),
	    count(input_packets, S);
	false ->
	    S1 = count(input_packets, S),
	    count(filter_packets, S1)
    end.

input_packet(Packet, {undefined, Pid, _If}) when is_pid(Pid) ->
    Pid ! Packet;
input_packet(Packet,{Module, undefined, _If}) when is_atom(Module) ->
    Module:input(Packet);
input_packet(Packet,{Module, Pid, _If}) when is_atom(Module), is_pid(Pid) ->
    Module:input(Pid,Packet).

count(Counter,S) ->
    nmea_2000_counter:update(Counter, 1),
    S.

encode_message(Command, Data) ->
    Len   = byte_size(Data),
    Sum   = -(Command+Len+sum(Data)),
    Data0 = <<Command,Len,Data/binary,Sum>>,
    Data1 = escape(Data0),
    <<?DLE,?STX,Data1/binary,?DLE,?ETX>>.

decode_message(Data) ->
    case unescape(Data) of
	<<?DLE,?STX,Command,Len,Data1:Len/binary,Sum,?DLE,?ETX>> ->
	    Check = (Command+Len+sum(Data1)+Sum) band 16#ff,
	    if Check =:= 0 ->
		    {ok,{Command,Data1}};
	       true ->
		    lager:warning("checksum error:~nData ~p~n"
				  "Command ~p, Len ~p, Data1 ~p, Sum ~p",
				  [Data, Command, Len, Data1, Sum]),
		    {error,bad_checksum}
	    end;
	<<?DLE,?STX,_Command,Len,Data1/binary>> ->
	    if Len < byte_size(Data1) ->
		    {error, message_too_long};
	       true ->
		    {error, message_too_short}
	    end;
	<<?DLE,?STX,_Data1/binary>> ->
	    {error, message_too_short}
    end.

escape(Data) ->
    list_to_binary(join(binary:split(Data, <<?DLE>>, [global]), <<?DLE,?DLE>>)).

unescape(Data) ->
    list_to_binary(join(binary:split(Data, <<?DLE,?DLE>>, [global]), <<?DLE>>)).

join([H],_Sep) -> [H];
join([H|T],Sep) -> [H,Sep|join(T,Sep)];
join([],_Sep) -> [].

sum(Bin) -> sum(Bin,0).
sum(<<C,Bin/binary>>, Sum) -> sum(Bin, Sum+C);
sum(<<>>, Sum) -> Sum.

raise_alarm(Alarm, Name, DeviceName, Reason, Extra) ->
    elarm:raise(Alarm, ?SUBSYS,
		[{id, Name}, {device, DeviceName},
		 {timestamp, timestamp()},
		 {reason, Reason}] ++ Extra).

call(Pid, Request) when is_pid(Pid) -> 
    gen_server:call(Pid, Request);
call(Id, Request) when is_integer(Id); is_list(Id) ->
    Interface = case Id of
		    Name when is_list(Name) -> Name;
		    I when is_integer(I) -> {?MODULE, I}
		end,
    case nmea_2000_router:interface_pid(Interface)  of
	Pid when is_pid(Pid) -> gen_server:call(Pid, Request);
	Error -> Error
    end.
	    
timestamp() ->
    TS = 
	try erlang:system_time(micro_seconds)
	catch
	    error:undef ->
		{MS,S,US} = os:timestamp(),
		(MS*1000000+S)*1000000+US
	end,
    lists:flatten(exo_http:format_timestamp(TS)).
