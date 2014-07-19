%%%-------------------------------------------------------------------
%%% @author Temple
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Jul 2014 17:09
%%%-------------------------------------------------------------------
-author("Temple").

%% API
-export([]).

-record(sensor, {id, cx_id, name, vl, fanout_ids}).
-record(actuator,{id, cx_id, name, vl, fanin_ids}).
-record(neuron, {id, cx_id, af, input_idps, output_ids}).
-record(cortex, {id, sensor_ids, actuator_ids, nids}).
