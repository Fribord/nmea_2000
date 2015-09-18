%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    PGN soft filter
%%% @end
%%% Created : 28 Aug 2015 by Tony Rogvall <tony@rogvall.se>

-module(nmea_2000_filter).

-export([new/0, add/2, del/2, get/1, list/1]).
-export([input/2]).

-include("../include/nmea_2000.hrl").

-record(nmea_fs,
	{
	  set :: term() %% sets
	}).

%% create filter structure
new() ->
    #nmea_fs { set = sets:new() }.

%% add filter to filter structure
add(PGNs, Fs) when is_list(PGNs), is_record(Fs, nmea_fs) ->
    Set = sets:from_list(PGNs),
    Set1 = sets:union(Fs#nmea_fs.set, Set),
    Fs#nmea_fs { set = Set1 }.

del(PGNs, Fs) when is_list(PGNs), is_record(Fs, nmea_fs) ->
    Set = sets:from_list(PGNs),
    Set1 = sets:subtract(Fs#nmea_fs.set, Set),
    Fs#nmea_fs { set = Set1 }.

get(Fs) when is_record(Fs, nmea_fs) ->
    {ok, sets:to_list(Fs#nmea_fs.set)}.

%% return the ordered filter list [{Num,#can_filter{}}]
list(Fs) when is_record(Fs, nmea_fs) ->
    {ok, sets:to_list(Fs#nmea_fs.set)}.

%% filter a frame
%% return true for no filtering (pass through)
%% return false for filtering
%%
input(P, Fs) when is_record(P, nmea_packet), is_record(Fs, nmea_fs) ->
    case sets:size(Fs#nmea_fs.set) of
	0 -> true;  %% default to accept all
	_ -> sets:is_element(P#nmea_packet.pgn, Fs#nmea_fs.set)
    end.

