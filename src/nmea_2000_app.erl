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
%%% NMEA 2000 application.
%%%
%%% Created:  9 Sep 2015 by Tony Rogvall
%%% @end
%%%-------------------------------------------------------------------
-module(nmea_2000_app).

-behaviour(application).

%% Application API
-export([start/2, 
	 stop/1]).
-export([config_change/3]).

%% Shortcut API
-export([start/0,
	 stop/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the nmea_2000 application.<br/>
%% Arguments are ignored, instead the options for the application servers are 
%% retreived from the application environment (sys.config).
%%
%% @end
%%--------------------------------------------------------------------
-spec start(StartType:: normal | 
			{takeover, Node::atom()} | 
			{failover, Node::atom()}, 
	    StartArgs::term()) -> 
		   {ok, Pid::pid()} |
		   {ok, Pid::pid(), State::term()} |
		   {error, Reason::term()}.

start(_StartType, _StartArgs) ->
    lager:info("arguments ignored.\n", []),
    nmea_2000_sup:start_link([]).


%%--------------------------------------------------------------------
%% @doc
%% Stops the application.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(State::term()) -> ok | {error, Error::term()}.

stop(_State) ->
    exit(normal).

%%--------------------------------------------------------------------
%% @doc
%% application changed config callback
%%
%% @end
%%--------------------------------------------------------------------
-spec config_change(Changed::list(),New::list(),Removed::list()) -> ok.
			   
config_change(Changed,New,Removed) ->
    nmea_2000_router:config_change(Changed,New,Removed).

%% ===================================================================
%% Test support
%% ===================================================================

%% @private
start() ->
    application:ensure_all_started(nmea_2000).

%% @private
stop() ->
    call([can, uart, eapi, ale, lager],
	 stop).

call([], _F) ->
    ok;
call([App|Apps], F) ->
    lager:info("~p: ~p\n", [F,App]),
    case application:F(App) of
	{error,{not_started,App1}} ->
	    call([App1,App|Apps], F);
	{error,{already_started,App}} ->
	    call(Apps, F);
	ok ->
	    call(Apps, F);
	Error ->
	    Error
    end.
