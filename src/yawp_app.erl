-module(yawp_app).
-author('MohsenMoqadam@yahoo.com').
-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

-include("yawp.hrl").

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    ok = application:ensure_started(lager),

    yawp_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
