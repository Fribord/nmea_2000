-module(nmea_2000_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
	 start_link/1,
	 stop/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
    case supervisor:start_link({local, ?MODULE}, ?MODULE, Args) of
	{ok, Pid} ->
	    {ok, Pid, {normal, Args}};
	Error -> 
	    Error
    end.

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

stop() ->
    exit(normal).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Args) ->
    Router = {nmea_2000_router, {nmea_2000_router, start_link, [Args]},
	      permanent, 5000, worker, [nmea_2000_router]},
    IfSup = {nmea_2000_if_sup, {nmea_2000_if_sup, start_link, []},
	     permanent, 5000, worker, [nmea_2000_if_sup]},
    {ok,{{one_for_all,3,5}, [Router, IfSup]}}.
