-module(yawp_utils).
-author('MohsenMoqadam@yahoo.com').

-export([get_key/2,
         get_key/3,
         send_signal/1,
         send_sync_signal/1]).

-include("yawp.hrl").

get_key(Key, Confs) ->
    get_key(Key, Confs, undefined).

get_key(Key, PropList, DefaultValue) ->
    case proplists:get_value(Key, PropList, DefaultValue) of
        undefined ->
            {error, undefined};
        DefaultValue ->
            {ok, DefaultValue};
        Value ->
            {ok, Value}
        end.

send_signal(#yawp_signal{receivers = Receivers} = Signal) ->
    [gen_server:cast(Pid, Signal) || Pid <- Receivers],
    ok.

send_sync_signal(#yawp_signal{receivers = [PidOrName]} = Signal) ->
    gen_server:call(PidOrName, Signal).
