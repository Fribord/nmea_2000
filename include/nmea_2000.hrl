-ifndef(__NMEA_2000_HRL__).
-define(__NMEA_2000_HRL__, true).

-record(nmea_packet,
	{
	  pgn :: 0..131071,  %% 17 bit packet pgn
	  intf = 0 :: integer(), %% input/output interface number
	  order :: 0..7,     %% packet order number
	  index :: 0..31,    %% last stored index
	  prio  :: 0..7,     %% current prio
	  src   :: 0..255,   %% source address
	  dst   :: 0..255,   %% destination address
	  len   :: 0..223,   %% current length, max = 6+31*7
	  totlen :: 0..223,  %% expected length
	  data = [] :: binary() | [binary()] %% list of frame fragments 
	}).

-endif.
