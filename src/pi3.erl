%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Dump a system pi3 
%%% @end
%%% Created : 14 Sep 2015 by Tony Rogvall <tony@rogvall.se>

-module(pi3).

-compile(export_all).

file(File) ->
    file(File, "pi3_nmea.log").

file(File,LogFile) ->
    case file:open(File, [read, binary]) of
	{ok,Fd} ->
	    {ok,Out} = file:open(LogFile, [write]),
	    T0 = calendar:datetime_to_gregorian_seconds({date(),time()})*1000,
	    try loop(Fd, Out,0, T0, dict:new()) of
		R -> R
	    catch
		error:Reason ->
		    io:format("error:~p\n~p\n", [Reason,
						 erlang:get_stacktrace()]),
		    {error, Reason}
	    after
		file:close(Fd),
		file:close(Out)
	    end;
	Error ->
	    Error
    end.

loop(Fd, Out, TimeMs, T0, Dict) ->
    case file:read(Fd, 12) of
	eof ->
	    ok;
	{ok,Data=(<<16#ff,ID,Frame:9/binary,_Sum>>)} ->
	    case checksum(Data) of
		true ->
		    Es = decode(ID,Frame),
		    Dict1 = update(Es, Dict),
		    TimeMs1 = TimeMs+10,
		    emit_nmea_frames(Out, TimeMs1, T0, Dict1),
		    io:format("air_temp:~w\n", [get_value(air_temperature,Dict1,0)]),
		    loop(Fd, Out, TimeMs1, T0, Dict1);
		false ->
		    io:format("~w: checksum error\n", [TimeMs]),
		    loop(Fd, Out, TimeMs+10, T0, Dict)
	    end;
	Error ->
	    Error
    end.

update([{Key,Value}|KVs], Dict) ->
    update(KVs, dict:store(Key, Value, Dict));
update([], Dict) ->
    Dict.

emit_nmea_frames(Out, TimeMs, T0, Dict) ->
    if TimeMs rem 100 =:= 0 ->
	    emit_frame(Out, 127488, T0+TimeMs, Dict);
       true ->
	    ok
    end,
    if TimeMs rem 1000 =:= 0 ->
	    emit_frame(Out, 127493, T0+TimeMs, Dict);
       true ->
	    ok
    end,
    if TimeMs rem 500 =:= 0 ->
	    emit_frame(Out, 127489, T0+TimeMs, Dict);
       true ->
	    ok
    end.

%% Dump as NMEA 2000 data 
%% 1 millibar = 1 hPa
%% 0 celsius  = 273.15 K
%%
%% PGN 127488 - engineParametersRapidUpdate  - 10 Hz
%%   engineInstance      = 0
%%   engineSpeed         = engine_speed 
%%   engineBoostPressure = manifold_pressure
%%   engineTiltTrim      = 0 ??
%%

emit_frame(Out, PGN=127488, TimeMs, Dict) ->
    EngineInstance=0,
    EngineSpeed=get_value(engine_speed, Dict, 0),
    EngineBoostPressure=get_value(manifold_pressure,Dict,0),
    EngineTiltTrim=0,
    Bin = co_codec:encode([EngineInstance,EngineSpeed,EngineBoostPressure,
			    EngineTiltTrim],
			   [{unsigned,8},{unsigned,16},{unsigned,16},{integer,8}]),
    Pri = 1,
    Src = 3,
    Dst = 255,
    Len = byte_size(Bin),
    io:format(Out, "~s,~w,~w,~w,~w,~w,~s\n",
	      [format_timestamp(TimeMs),
	       Pri,PGN,Src,Dst,Len,
	       format_data(Bin)]);
%% PGN 127489 - engineParametersDynamic - 2Hz
%%   engineInstance      = 0
%%   oilPressure         = oil_pressure
%%   oilTemperature*100  = oil_temperature + 273.15
%%   temperature*100     = air_temperature + 273.15
%%   alternatorPotential*100 = ?
%%   fuelRate*10         = ?
%%   totalEngineHours    = ?
%%   coolantPressure     = ?
%%   fuelPressure        = fuel_pressure
%%   discreteStatus1     = ?
%%   discreteStatus2     = ?
%%   percentEngineLoad   = ?
%%   percentEngineTorque = ?
emit_frame(Out,PGN=127489, TimeMs, Dict) ->
    EngineInstance=0,
    OilPressure=get_value(oil_pressure, Dict, 0),
    OilTemperature = trunc((get_value(oil_temperature,Dict,0) + 273.15)*100),
    Temperature = trunc((get_value(air_temperature,Dict,0) + 273.15)*100),
    AlternatorPotential = 0*100,
    FuelRate = 0*10,
    TotalEngineHours = 0,
    CoolantPressure = 0,
    FuelPressure = get_value(fuel_pressure, Dict, 0),
    DiscreteStatus1 = 0,
    DiscreteStatus2 = 0,
    PercentEngineLoad = 0,
    PercentEngineTorque = 0,
    Bin = co_codec:encode([EngineInstance,OilPressure,OilTemperature,Temperature,AlternatorPotential,FuelRate,TotalEngineHours,CoolantPressure,FuelPressure,0,DiscreteStatus1,DiscreteStatus2,PercentEngineLoad,PercentEngineTorque], 
			  [{unsigned,8},{unsigned,16},{unsigned,16},{unsigned,16},{unsigned,16},{integer,16},{unsigned,32},{unsigned,16},{unsigned,16},{unsigned,8},{unsigned,16},{unsigned,16},{integer,8},{integer,8}]),    
    Pri = 1,
    Src = 3,
    Dst = 255,
    Order = 1,
    emit_fast(Out,TimeMs,Pri,PGN,Src,Dst,Order,Bin);

%% PGN 127493  - transmissionParametersDynamic - 1Hz
%%   engineInstance      = 0
%%   transmissionGear    = gear_position
%%   oilPressure         = oil_pressure
%%   oilTemperature*100  = oil_temperature + 273.15
%%   discreteStatus1     = ?
%%
emit_frame(Out,PGN=127493,TimeMs,Dict) ->
    EngineInstance=0,
    TransmissionGear=get_value(gear_position,Dict,0),
    OilPressure=get_value(oil_pressure, Dict, 0),
    OilTemperature = trunc((get_value(oil_temperature,Dict,0) + 273.15)*100),
    DiscreteStatus1 = 0,
    Bin = co_codec:encode([EngineInstance,TransmissionGear,0,OilPressure,OilTemperature,DiscreteStatus1,0],
			   [{unsigned,8},{unsigned,2},{unsigned,6},
			    {unsigned,16},{unsigned,16},{unsigned,8},
			    {unsigned,8}]),
    Pri = 1,
    Src = 3,
    Dst = 255,
    Len = byte_size(Bin),
    io:format(Out,"~s,~w,~w,~w,~w,~w,~s\n",
	      [format_timestamp(TimeMs),
	       Pri,PGN,Src,Dst,Len,
	       format_data(Bin)]).



emit_fast(Fd,TimeMs,Pri,PGN,Src,Dst,Order,
	  Bin = <<Fast:6/binary,Data/binary>>) ->
    Len = byte_size(Bin),
    io:format(Fd,"~s,~w,~w,~w,~w,~w,~s\n",
	      [format_timestamp(TimeMs),
	       Pri,PGN,Src,Dst,8,
	       format_data(<<Order:3,0:5,Len,Fast/binary>>)]),
    emit_fast_(Fd,TimeMs,Pri,PGN,Src,Dst,Order,1,Data).
    
emit_fast_(_Fd,_TimeMs,_Pri,_PGN,_Src,_Dst,_Order,_Index,<<>>) ->
    ok;
emit_fast_(Fd,TimeMs,Pri,PGN,Src,Dst,Order,Ix,<<Fast:7/binary,Data/binary>>) ->
    io:format(Fd,"~s,~w,~w,~w,~w,~w,~s\n",
	      [format_timestamp(TimeMs),
	       Pri,PGN,Src,Dst,8,
	       format_data(<<Order:3,Ix:5,Fast/binary>>)]),
    emit_fast_(Fd,TimeMs,Pri,PGN,Src,Dst,Order,Ix+1,Data);
emit_fast_(Fd,TimeMs,Pri,PGN,Src,Dst,Order,Ix,Fast) ->
    Len = byte_size(Fast)+1,
    io:format(Fd,"~s,~w,~w,~w,~w,~w,~s\n",
	      [format_timestamp(TimeMs),
	       Pri,PGN,Src,Dst,Len,
	       format_data(<<Order:3,Ix:5,Fast/binary>>)]).

format_timestamp(TimeMs) ->
    GSec = TimeMs div 1000,
    Ms  = TimeMs rem 1000,
    {{Year,Mon,Day},{H,M,S}} = calendar:gregorian_seconds_to_datetime(GSec),
    io_lib:format("~4..0w-~2..0w-~2..0w-~2..0w:~2..0w:~2..0w.~3..0w",
		  [Year,Mon,Day,H,M,S,Ms]).

get_value(Key, Dict, Default) ->
    case dict:find(Key, Dict) of
	error -> Default;
	{ok,Value} -> Value
    end.
	    

format_data(<<X>>) ->
    [hex(X bsr 4),hex(X)];
format_data(<<X,Bin/binary>>) ->
    [hex(X bsr 4),hex(X),$, | format_data(Bin)].

hex(X) ->
    element((X band 15)+1, {$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$a,$b,$c,$d,$e,$f}).
    

%% Decode each type of frame  0-9
decode(0,<<RPM:16, TPS:8, MAP:16, LAM1:8, LAM1T:8, LAM2:8, LAM2T:8>>) ->
    [{engine_speed,RPM}, {tps, TPS/2}, {manifold_pressure, MAP},
     {lambda1, LAM1/100}, {lambda1_trim, LAM1T/1.28},
     {lambda2, LAM2/100}, {labbda2_trim, LAM2T/1.28}];

decode(1,<<SPD:16, _Sp1:16, DETG:8, TJ:16, SA:16>>) ->
    [{wheel_speed,SPD*0.06},{detonation_average,DETG/2.55},
     {fuel_pulse_width, TJ/250}, {ignition_angle, (SA-360)/4}];

decode(2,<<SPD:16, _Sp1:16, DETG:8, EOP:8, FP:8, GPOS:8, _Sp2:8>>) ->
    [{wheel_speed,SPD*0.06},{detonation_average,DETG/2.55},
     {oil_pressure,EOP*50}, {fuel_pressure,FP*50},
     {gear_position,GPOS}];

decode(3,<<SPD:16, _Sp1:16, DETG:8, VBAT:8, _Sp4:8, _Sp3:16>>) ->
    [{wheel_speed,SPD*0.06},{detonation_average,DETG/2.55},
     {battery_voltage, VBAT/10}];

decode(4,<<SPD:16, _Sp1:16, DETG:8, ERR:16, _Sp3:16>>) ->
    [{wheel_speed,SPD*0.06},{detonation_average,DETG/2.55},
     {error_flag, ERR}];

decode(5,<<SPD:16, _Sp1:16, DETG:8, ACT:8, ECT:8, _Sp5:16>>) ->
    [{wheel_speed,SPD*0.06},{detonation_average,DETG/2.55},
     {air_temperature,ACT-55}, {water_temperature,ECT-55}];

decode(6,<<SPD:16, _Sp1:16, DETG:8, EOT:8, FT:8, _Sp6:16>>) ->
    [{wheel_speed,SPD*0.06},{detonation_average,DETG/2.55},
     {oil_temperature,EOT-55}, {fule_temperature,FT-55}];

decode(7,<<SPD:16, _Sp1:16, DETG:8, BAP:16, _Sp7:16>>) ->
    [{wheel_speed,SPD*0.06},{detonation_average,DETG/2.55},
     {barometric_pressure, BAP}];

decode(8,<<SPD:16, _Sp1:16, DETG:8, FC:16, _Sp8:16>>) ->
    [{wheel_speed,SPD*0.06},{detonation_average,DETG/2.55},
     {fule_consumption,FC/100}];

decode(9,<<SPD:16, _Sp1:16, DETG:8, TEX:8, DETI:8, CLS:16>>) ->
    [{wheel_speed,SPD*0.06},{detonation_average,DETG/2.55},
     {exaust_temperature,TEX*5},
     {det_ignition_correction,DETI/4-32},
     {change_light_speed,CLS}].

checksum(<<Data:11/binary,Sum>>) ->
    (lists:sum([X || <<X>> <= Data]) band 16#ff) =:= Sum.

