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
%%% @author Malotte W Lönne <malotte@malotte.net>
%%% @doc
%%%    Nmea PGN counter. For test.
%%% Created : May 2017 by Malotte W Lönne
%%% @end
-module(nmea_2000_pgn_counter).
-behaviour(gen_server).

-include_lib("nmea_2000/include/nmea_2000.hrl").

%% general api
-export([start/1,
	 start/0,
	 stop/0]).

%% functional api
%% -export([]).

%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

%% test api
-export([dump/0]).

-define(SERVER, ?MODULE). 
-define(DEFAULT_DURATION, 60). %% seconds

%% For dialyzer
-type start_options()::{linked, TrueOrFalse::boolean()} |
		       {duration, Duration::integer()} |
		       {interval, Interval::integer()}.

%% Loop data
-record(ctx,
	{
	  state = init::atom(),
	  interval = 0::integer(),
	  pgn_table = []::list({Pgn::integer(), Counter::integer()})
	}).
-ifdef(DBG).
-define(dbg(Msg), io:format(Msg)).
-define(dbg(Format, Args), io:format(Format, Args)).
-else.
-define(dbg(Msg), ok).
-define(dbg(Format, Args), ok).
-endif.

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server.
%% Loads configuration from File.
%% @end
%%--------------------------------------------------------------------
-spec start() -> {ok, Pid::pid()} | 
		 ignore | 
		 {error, Error::term()}.

start() ->
    ?dbg("~p: start~n", [?MODULE]),
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

-spec start(Opts::list(start_options())) -> 
			{ok, Pid::pid()} | 
			ignore | 
			{error, Error::term()}.

start(Opts) ->
    ?dbg("~p: start: args = ~p\n", [?MODULE, Opts]),
    gen_server:start({local, ?SERVER}, ?MODULE, Opts, []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok | {error, Error::term()}.

stop() ->
    gen_server:call(?SERVER, stop).


%%--------------------------------------------------------------------
%% @doc
%% Dumps data to standard output.
%%
%% @end
%%--------------------------------------------------------------------
-spec dump() -> ok | {error, Error::atom()}.

dump() ->
    gen_server:call(?SERVER,dump).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Args::list(start_options())) -> 
		  {ok, Ctx::#ctx{}} |
		  {stop, Reason::term()}.

init(Args) ->
    ?dbg("args = ~p,\n pid = ~p\n", [Args, self()]),
    Duration =  proplists:get_value(duration,Args,?DEFAULT_DURATION),
    io:format("Will execute ~p seconds.~n", [Duration]),
    erlang:start_timer(Duration * 1000, self(), stop),
    case proplists:get_value(interval,Args,0) of
	I when I > 0 ->
	    io:format("Will report with ~p seconds interval.~n", [I]),
	    erlang:start_timer(I * 1000, self(), {interval, I});
	_I -> ok
    end,
    nmea_2000_router:attach(),
    {ok, #ctx {}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages.
%% Request can be the following:
%% <ul>
%% <li> dump - Writes loop data to standard out (for debugging).</li>
%% <li> stop - Stops the application.</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-type call_request()::
	dump |
	stop.

-spec handle_call(Request::call_request(), From::{pid(), Tag::term()}, Ctx::#ctx{}) ->
			 {reply, Reply::term(), Ctx::#ctx{}} |
			 {noreply, Ctx::#ctx{}} |
			 {stop, Reason::atom(), Reply::term(), Ctx::#ctx{}}.

handle_call(dump, _From, Ctx=#ctx {pgn_table = PgnTable}) ->
    print_table(PgnTable),
    {reply, ok, Ctx};

handle_call(stop, _From, Ctx=#ctx {pgn_table = PgnTable}) ->
    print_table(PgnTable),
    ?dbg("stop.",[]),
    {stop, normal, ok, Ctx};

handle_call(_Request, _From, Ctx) ->
    ?dbg("unknown request ~p.", [_Request]),
    {reply, {error,bad_call}, Ctx}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages.
%%
%% @end
%%--------------------------------------------------------------------
-type cast_msg()::
	term().

-spec handle_cast(Msg::cast_msg(), Ctx::#ctx{}) -> 
			 {noreply, Ctx::#ctx{}} |
			 {stop, Reason::term(), Ctx::#ctx{}}.

handle_cast(_Msg, Ctx) ->
    ?dbg("unknown msg ~p.", [_Msg]),
    {noreply, Ctx}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages.
%% 
%% @end
%%--------------------------------------------------------------------
-type info()::
	term().

-spec handle_info(Info::info(), Ctx::#ctx{}) -> 
			 {noreply, Ctx::#ctx{}} |
			 {noreply, Ctx::#ctx{}, Timeout::timeout()} |
			 {stop, Reason::term(), Ctx::#ctx{}}.

handle_info(Packet, Ctx)
  when is_record(Packet, nmea_packet) ->
    ?dbg("Packet ~p.", [Packet]),
    {noreply, handle_packet(Packet, Ctx)};

handle_info({timeout,_TRef,{interval, I}},Ctx=#ctx {pgn_table = PgnTable}) ->
    print_table(PgnTable),
    erlang:start_timer(I * 1000, self(), {interval, I}),
    {noreply, Ctx#ctx {pgn_table = []}};

handle_info({timeout,_TRef,stop},Ctx=#ctx {pgn_table = PgnTable}) ->
    print_table(PgnTable),
    {stop, normal, Ctx};

handle_info(_Info, Ctx) ->
    ?dbg("unknown info ~p.", [_Info]),
    {noreply, Ctx}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec terminate(Reason::term(), Ctx::#ctx{}) -> 
		       no_return().

terminate(_Reason, _Ctx=#ctx {state = State}) ->
    ?dbg("terminating in state ~p, reason = ~p.",
	 [State, _Reason]),
    ok.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process ctx when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn::term(), Ctx::#ctx{}, Extra::term()) -> 
			 {ok, NewCtx::#ctx{}}.

code_change(_OldVsn, Ctx, _Extra) ->
    ?dbg("old version ~p.", [_OldVsn]),
    {ok, Ctx}.


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
handle_packet(Packet=#nmea_packet {pgn = Pgn}, Ctx=#ctx {pgn_table = PgnTable}) ->
    ?dbg("src:~w pgn:~w totlen=~w, len=~w data=~p\n", 
		[Packet#nmea_packet.src,Packet#nmea_packet.pgn,
		 Packet#nmea_packet.totlen,Packet#nmea_packet.len,
		 Packet#nmea_packet.data]),
    NewPgnTable = 
	case lists:keytake(Pgn, 1, PgnTable) of
	    false -> [{Pgn, 1} | PgnTable];
	    {value, {Pgn, Counter}, Rest} ->  [{Pgn, Counter + 1} | Rest]
	end,
    Ctx#ctx {pgn_table = NewPgnTable}.
    
print_table(PgnTable) ->
    io:format("Counted PGN:s~n"),
    lists:foreach(fun({Pgn, Counter}) ->
			  io:format("~p: ~p~n", [Pgn, Counter])
		  end, PgnTable).
