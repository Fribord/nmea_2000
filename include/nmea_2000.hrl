%%%---- BEGIN COPYRIGHT --------------------------------------------------------
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
%%%---- END COPYRIGHT ----------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%% File    : nmea_2000.hrl
%%% Description : nmea 2000 application definitions
%%% Created : September 2015 by Tony Rogvall
%%% @end
-ifndef(__NMEA_2000_HRL__).
-define(__NMEA_2000_HRL__, true).

-record(nmea_packet,
	{
	  pgn :: 0..131071,  %% 17 bit packet pgn
	  order :: 0..255,   %% packet order number
	  index :: 0..31,    %% last stored index
	  prio  :: 0..7,     %% current prio
	  src   :: 0..255,   %% source address
	  dst   :: 0..255,   %% destination address
	  len   :: 0..223,   %% current length, max = 6+31*7
	  totlen :: 0..223,  %% expected length
	  data = [] :: binary() | [binary()] %% list of frame fragments 
	}).

-endif.
