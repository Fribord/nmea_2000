%%% coding: latin-1
%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2015, Rogvall Invest AB, <tony@rogvall.se>
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
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Read NMEA/CAN log files + work as a backend to nmea_2000_router
%%%
%%% Created :  7 Sep 2015 by Tony Rogvall 
%%% @end
%%%-------------------------------------------------------------------

-module(nmea_2000_log).

-include_lib("lager/include/log.hrl").
-include("../include/nmea_2000.hrl").
-include_lib("can/include/can.hrl").

-behaviour(gen_server).

-define(is_digit(X), (((X) >= $0) andalso ((X) =< $9))).

%% NMEA 2000 router API
-export([start/0, start/1, start/2]).
-export([start_link/0, start_link/1, start_link/2]).
-export([stop/1]).

%% Direct log API
-export([open/1, close/1]).
-export([read/1, read_can_frame/1]).
-export([transmit/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Test API
-export([pause/1, resume/1, restart/1]).

-record(s, {
	  receiver={nmea_2000_router, undefined, 0} ::
	    {Module::atom(), %% Module to join and send to
	     Pid::pid(),     %% Pid if not default server
	     If::integer()}, %% Interface id
	  file,              %% file name
	  fd,                %% open file descriptor
	  max_rate,        %% Max read frequency
	  retry_interval,  %% Timeout for open retry
	  retry_timer,     %% Timer reference for retry
	  read_timer,      %% Timer for reading data entries
	  rotate = true,   %% Rotate or run once
	  paused = false,  %% Pause input
	  pgn_dict,        %% Place where PGNs are stored
	  last_ts,         %% last time
	  fs               %% nmea_2000_filter:new()
	 }).

-type nmea_2000_log_option() ::
	{router,    RouterName::atom()} |
	{receiver,  ReceiverPid::pid()} |
	{file,      FileName::string()} |   %% Log file name
	{max_rate,  MaxRate::integer()} |   %% Hz
	{retry_interval, ReopenTimeout::timeout()} |
	{rotate, Rotate::boolean()} |
	{accept, Accept::list(atom())} |
	{reject, Accept::list(atom())} |
	{default, accept | reject}.

-define(SERVER, ?MODULE).

-define(DEFAULT_RETRY_INTERVAL,  2000).
-define(DEFAULT_MAX_RATE,        10).    %% 10 Hz
-define(DEFAULT_IF,              0).

%%%===================================================================
%%% API
%%%===================================================================
-spec start() -> {ok,pid()} | {error,Reason::term()}.
start() ->
    start(1,[]).

-spec start(BudId::integer()) -> {ok,pid()} | {error,Reason::term()}.
start(BusId) ->
    start(BusId,[]).

-spec start(BudId::integer(),Opts::[nmea_2000_log_option()]) ->
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

-spec start_link(BusId::integer(),Opts::[nmea_2000_log_option()]) ->
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

-spec pause(BusId::integer()) -> ok | {error, Error::atom()}.
pause(BusId) when is_integer(BusId) ->
    gen_server:call(server(BusId), pause).
-spec resume(BusId::integer()) -> ok | {error, Error::atom()}.
resume(BusId) when is_integer(BusId) ->
    gen_server:call(server(BusId), resume).
-spec restart(BusId::integer()) -> ok | {error, Error::atom()}.
restart(BusId) when is_integer(BusId) ->
    gen_server:call(server(BusId), restart).

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
    lager:debug("options ~p", [Opts]),
    Router = proplists:get_value(router, Opts, nmea_2000_router),
    Pid = proplists:get_value(receiver, Opts, undefined),
    RetryInterval = proplists:get_value(retry_interval,Opts,
					?DEFAULT_RETRY_INTERVAL),
    MaxRate0 = proplists:get_value(max_rate,Opts,?DEFAULT_MAX_RATE),
    MaxRate = if is_number(MaxRate0), MaxRate0 > 0 -> MaxRate0;
		 true -> ?DEFAULT_MAX_RATE
	      end,
    Accept = proplists:get_value(accept, Opts, []),
    Reject = proplists:get_value(reject, Opts, []),
    Default = proplists:get_value(default, Opts, accept),
    Rotate = proplists:get_value(rotate, Opts, true),

    File = proplists:get_value(file, Opts),

    if File =:= undefined ->
	    ?error("nmea_2000_log: missing file argument"),
	    {stop, einval};
       true ->
	    LogFile = nmea_2000_log:text_expand(File,[]),
	    case join(Router, Pid, {?MODULE,File,Id}) of
		{ok, If} when is_integer(If) ->
		    ?debug("nmea_2000_log:joined: intf=~w", [If]),
		    S = #s{ receiver={Router,Pid,If},
			    file = LogFile,
			    pgn_dict = dict:new(),
			    max_rate = MaxRate,
			    retry_interval = RetryInterval,
			    rotate = Rotate,
			    fs=nmea_2000_filter:new(Accept,Reject,Default)
			  },
		    ?info("nmea_2000_log: using file ~s\n", [LogFile]),
		    case open_logfile(S) of
			{ok, S1} -> 
			    erlang:register(server(Id), self()),
			    {ok, S1};
			Error -> 
			    {stop, Error}
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

handle_call({send,_Packet}, _From, S) ->
    {reply, {error, read_only}, S};
handle_call(statistics,_From,S) ->
    {reply,{ok,nmea_2000_counter:list()}, S};
handle_call(pause, _From, S) ->
    {reply, ok, S#s {paused = true}};
handle_call(resume, _From, S) ->
    {reply, ok, S#s {paused = false}};
handle_call(restart, _From, S) ->
    {reply, ok, reopen_logfile(S)};
handle_call(stop, _From, S) ->
    {stop, normal, ok, S};
handle_call(_Request, _From, S) ->
    ?debug("got unknown request ~p\n", [_Request]),
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
handle_cast({send,_Packet}, S) ->
    {noreply, S};
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
handle_cast(_Msg, S) ->
    ?debug("got unknown msg ~p\n", [_Msg]),
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

handle_info({timeout,TRef,reopen},S) when TRef =:= S#s.retry_timer ->
    case open_logfile(S#s { retry_timer = undefined }) of
	{ok, S1} ->
	    {noreply, S1};
	Error ->
	    {stop, Error, S}
    end;

handle_info({timeout,_Ref,read},S) when S#s.paused =:= true ->
    %% Restart timer
    Timer = start_timer(100, read),
    {noreply, S#s { read_timer = Timer }};

handle_info({timeout,Ref,read},S) when Ref =:= S#s.read_timer ->
    if S#s.fd =/= undefined ->
	    case read_can_frame(S#s.fd) of
		eof ->
		    case S#s.rotate of
			true ->
			    {ok,0} = file:position(S#s.fd, 0),
			    Timer = start_timer(100, read),
			    {noreply, S#s { read_timer = Timer, 
					    last_ts = undefined }};
			false ->
			    close(S#s.fd),
			    {noreply, S#s { fd = undefined,
					    last_ts = undefined }}
		    end;
		error ->
		    lager:warning("read_can_frame go error (corrupt file?)",[]),
		    {noreply, reopen_logfile(S)};
		CanFrame ->
		    Fun = fun(Packet) -> input(Packet, S) end,
		    Dict = nmea_2000_packet:collect_packet(CanFrame,
							   Fun,
							   S#s.pgn_dict),
		    Ts = if CanFrame#can_frame.ts =:= ?CAN_NO_TIMESTAMP -> 0;
			    true -> CanFrame#can_frame.ts
			 end,
		    LastTs = if S#s.last_ts =:= undefined -> Ts;
				true -> S#s.last_ts
			     end,
		    Td = max(Ts - LastTs, trunc((1/S#s.max_rate)*1000)),
		    Timer = start_timer(Td, read),
		    lager:debug("read timer ~p", [Td]),
		    {noreply, S#s { pgn_dict = Dict, last_ts = Ts,
				    read_timer = Timer }}
	    end;
       true ->
	    {noreply, S}
    end;

handle_info(_Info, S) ->
    ?debug("got unknown info ~p", [_Info]),
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

open_logfile(S0=#s {file = File }) ->
    case open(File) of
	{ok,Fd} ->
	    ?debug("nmea_2000_log:open: ~s", [File]),
	    Timer = start_timer(100, read),
	    {ok, S0#s { fd = Fd, read_timer = Timer, last_ts = undefined }};
	{error,E} when E =:= eaccess; E =:= enoent ->
	    ?debug("nmea_2000_log:open: ~s  error ~w, will try again "
		   "in ~p msecs.", [File,E,S0#s.retry_interval]),
	    Timer = start_timer(S0#s.retry_interval, reopen),
	    {ok, S0#s { retry_timer = Timer }};
	Error ->
	    lager:error("nmea_2000_log: error ~w", [Error]),
	    Error
    end.

reopen_logfile(S) ->
    if S#s.fd =/= undefined ->
	    ?debug("closing file ~s", [S#s.file]),
	    R = close(S#s.fd),
	    ?debug("closed ~p", [R]),
	    R;
       true ->
	    ok
    end,
    Timer = start_timer(S#s.retry_interval, reopen),
    S#s { fd=undefined, pgn_dict = dict:new(), retry_timer=Timer }.

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
	{{Pri,PGN,Src,Dst},Len,FrameData,TimeStamp} ->
	    Ts = timestamp_to_ms(TimeStamp),
	    ID = nmea_2000_lib:encode_canid(Pri,PGN,Src,Dst) bor ?CAN_EFF_FLAG,
	    #can_frame { id = ID,
			 len = Len,
			 data = FrameData, 
			 ts = Ts }
    end.

transmit(File) ->
    {ok,Fd} = open(File),
    transmit_loop(Fd, 1, undefined, 10).

transmit_loop(Fd, I, LastTs, MaxRate) ->
    case read_can_frame(Fd) of
	eof ->
	    file:position(Fd, 0),
	    transmit_loop(Fd, I, LastTs, MaxRate);
	CanFrame ->
	    can:send(CanFrame),
	    io:format("."),
	    if I rem 79 =:= 0 -> io:format("\n");
	       true -> ok
	    end,
	    Ts = if CanFrame#can_frame.ts =:= ?CAN_NO_TIMESTAMP -> 0;
		    true -> CanFrame#can_frame.ts
		 end,
	    LastTs1 = if LastTs =:= undefined -> Ts;
			 true -> LastTs
		      end,
	    Td = max(Ts - LastTs1, trunc((1/MaxRate)*1000)),
	    timer:sleep(Td),
	    transmit_loop(Fd,I+1,LastTs1,MaxRate)
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
	    {nmea_2000_lib:decode_canid(CanID),Len,FrameData,undefined};
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
	    {nmea_2000_lib:decode_canid(CanID),Len,FrameData,undefined};
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
    TimeStamp = {{Year,Mon,Day},{Hour,Min,Sec},Milli},
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
    TimeStamp = {{Year,Mon,Day},{Hour,Min,Sec},Milli},
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
    TimeStamp = {{Year,Mon,Day},{Hour,Min,Sec},Milli},
    parse_log_data(Data, TimeStamp);
parse_log_2(_Other) ->
    io:format("read log_2: ~p\n", [_Other]),
    error.

parse_log_data(Data, TimeStamp) ->
    case binary:split(Data, <<",">>, [global,trim_all]) of
	[PriBin,PGNBin,SrcBin,DstBin,LenBin | Ds] ->
	    Pri = erlang:binary_to_integer(PriBin),
	    PGN = erlang:binary_to_integer(PGNBin),
	    Src = erlang:binary_to_integer(SrcBin),
	    Dst = erlang:binary_to_integer(DstBin),
	    Len = erlang:binary_to_integer(LenBin),
	    FrameData = list_to_binary(
			  [ erlang:binary_to_integer(D,16) || D <- Ds]),
	    {{Pri,PGN,Src,Dst},Len,FrameData,TimeStamp};
	_Other ->
	    io:format("read log_data: ~p\n", [_Other]),
	    error
    end.

timestamp_to_ms(undefined) -> ?CAN_NO_TIMESTAMP;
timestamp_to_ms({Date,{H,M,S},Milli}) ->
    Days = calendar:date_to_gregorian_days(Date),
    (((Days*24 + H)*60 + M)*60 + S)*1000 + Milli.

server(BusId) ->
    list_to_atom(atom_to_list(?SERVER) ++ integer_to_list(BusId)).
