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
%%% @author Malotte W Lönne <malotte@malotte.net>
%%% @doc
%%%    nmea 2000 server
%%% Created : 9 Sep 2015 by Tony Rogvall
%%% @end

-module(nmea_2000_srv).
-behaviour(gen_server).

-include_lib("lager/include/log.hrl").
-include_lib("can/include/can.hrl").
-include("../include/nmea_2000.hrl").

%% general api
-export([start_link/1, 
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

-define(dbg(F,A), ok).
%% -define(dbg(F,A), io:format((F),(A)).

-define(SERVER, ?MODULE). 

%% For dialyzer
-type start_options()::{linked, Linked::boolean()}.

%% Loop data
-record(ctx,
	{
	  state = init ::atom(),
	  dict ::dict:dict(),
	  engines = [] ::list({integer(),atom()})
	}).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server.
%% @end
%%--------------------------------------------------------------------
-spec start_link(Opts::list(start_options())) -> 
			{ok, Pid::pid()} | 
			ignore | 
			{error, Error::term()}.

start_link(Opts) ->
    lager:info("~p: start_link: args = ~p\n", [?MODULE, Opts]),
    F =	case proplists:get_value(linked,Opts,true) of
	    true -> start_link;
	    false -> start
	end,
    
    gen_server:F({local, ?SERVER}, ?MODULE, Opts, []).


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
    lager:info("~p: init: args = ~p,\n pid = ~p\n", [?MODULE, Args, self()]),
    can_counter:init(nmea_stat_in),   %% number of input packets received
    %%can_counter:init(nmea_stat_out),  %% number of output packets  sent
    can_counter:init(nmea_stat_err),  %% number of error packets received
    {ok, #ctx {dict = dict:new(), engines = application:get_env(nmea_2000, engines, [])}}.

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

handle_call({join, _Pid, _Param} = _Req, _From, Ctx) ->
    lager:debug("~p.", [_Req]),
    %% Should we do anything ?
    {reply, ok, Ctx};

handle_call(dump, _From, 
	    Ctx=#ctx {state = State, dict = Dict, engines = Engines}) ->
    lager:debug("dump.", []),
    io:format("Ctx:\nState = ~p.\n", [State]),
    io:format("Engines = ~p\n.", [Engines]),
    io:format("Dict = ~p\n.", [Dict]),
    {reply, ok, Ctx};

handle_call(stop, _From, Ctx) ->
    lager:debug("stop.",[]),
    {stop, normal, ok, Ctx};

handle_call(_Request, _From, Ctx) ->
    lager:debug("unknown request ~p.", [_Request]),
    {reply, {error,bad_call}, Ctx}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages.
%%
%% @end
%%--------------------------------------------------------------------
-type cast_msg()::
	{input, Pid::pid(), Frame::#can_frame{}}.

-spec handle_cast(Msg::cast_msg(), Ctx::#ctx{}) -> 
			 {noreply, Ctx::#ctx{}} |
			 {stop, Reason::term(), Ctx::#ctx{}}.

handle_cast({input, Pid, Frame} = _Msg, Ctx) 
  when is_pid(Pid), is_record(Frame, can_frame) ->
    lager:debug("~p.", [_Msg]),
    if ?is_can_frame_err(Frame) ->  %% FIXME: send to error handler
	    count(nmea_stat_err),
	    {noreply, Ctx};
       true ->
	    count(nmea_stat_in),
	    {noreply, process_frame(Frame, Ctx)}
    end;

handle_cast(_Msg, Ctx) ->
    lager:debug("unknown msg ~p.", [_Msg]),
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

handle_info(_Info, Ctx) ->
    lager:debug("unknown info ~p.", [_Info]),
    {noreply, Ctx}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec terminate(Reason::term(), Ctx::#ctx{}) -> 
		       no_return().

terminate(_Reason, _Ctx=#ctx {state = State}) ->
    lager:debug("terminating in state ~p, reason = ~p.",
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
    lager:debug("old version ~p.", [_OldVsn]),
    {ok, Ctx}.


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
count(Counter) ->
    can_counter:update(Counter, 1).

process_frame(Frame, Ctx=#ctx{dict = Dict, engines = Engines}) ->
    F = fun(P) ->
		lager:debug("src:~w pgn:~w totlen=~w, len=~w data=~p\n", 
		     [P#nmea_packet.src,P#nmea_packet.pgn,
		      P#nmea_packet.totlen,P#nmea_packet.len,
		      P#nmea_packet.data]),
		try nmea_2000_pgn:decode(P#nmea_packet.pgn,
					 P#nmea_packet.data) of
		    false ->
			lager:error("PGN:~w, data=~w\n",
				    [P#nmea_packet.pgn,
				     P#nmea_packet.data]);
		    Params ->
			emit(Params, Engines)
		catch
		    error:Reason ->
			lager:error("pgn:~w ~p, crash data=~p\n~p\n", 
				    [P#nmea_packet.pgn,
				     Reason,
				     P#nmea_packet.data,
				     erlang:get_stacktrace()])
		end
	end,
    Ctx#ctx {dict = nmea_2000_packet:collect_packet(Frame, F , Dict)}.

 
emit({transmissionParametersDynamic, FieldList} = _Param, Engines) ->
    %% Append 'Transmission' to make unique labels
    Fields = [{append(K, 'Transmission'), V} || {K, V} <- FieldList],
    emit1({transmissionParametersDynamic, Fields}, Engines);
emit(Param, Engines) ->
    emit1(Param, Engines).

emit1({_ParamName, FieldList} = Param, Engines) ->
    lager:debug("param ~p.", [Param]),
    EngineNo = proplists:get_value(engineInstance, FieldList),
    Engine = proplists:get_value(EngineNo, Engines),
    inform_hex(Engine, FieldList).
    
inform_hex(_Engine, []) ->
    ok;
inform_hex(Engine, [{engineInstance,_I} | Fields]) ->
    inform_hex(Engine, Fields);
inform_hex(Engine, [{engineInstanceTransmission,_I} | Fields]) ->
    inform_hex(Engine, Fields);
inform_hex(Engine, [{K,V} | Fields]) ->
    hex:inform('analog', [{label, K}, {equipment_id, Engine}, {value, V}]),
    inform_hex(Engine, Fields).
    
append(Atom, Suffix) ->
    list_to_atom(atom_to_list(Atom) ++ atom_to_list(Suffix)).
