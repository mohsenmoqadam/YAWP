-module(yawp_utils).

-author('MohsenMoqadam@yahoo.com').

-export([send_signal/1,
         send_sync_signal/1]).

-include("yawp.hrl").

-spec send_signal(yawp_signal()) -> ok.
send_signal(#yawp_signal{receivers = Receivers} = Signal) ->
    [gen_server:cast(Pid, Signal) || Pid <- Receivers],
    ok.

-spec send_sync_signal(yawp_signal()) -> term().
send_sync_signal(#yawp_signal{receivers = [PidOrName]} = Signal) ->
    gen_server:call(PidOrName, Signal).
