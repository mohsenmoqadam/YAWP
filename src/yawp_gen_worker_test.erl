-module(yawp_gen_worker_test).
-author('MohsenMoqadam@yahoo.com').
-behaviour(yawp_gen_worker).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         format_status/2]).

-include("yawp.hrl").
-define(SERVER, ?MODULE).

-record(state, {pool_name :: string()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Confs) ->
    yawp_gen_worker:start_link(?MODULE, Confs, []).

%%%===================================================================
%%% yawp_gen_worker callbacks
%%%===================================================================

init(PoolName, _Args) ->
    process_flag(trap_exit, true),
    {ok, #state{pool_name = PoolName}}.

handle_call(_Task, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Task, State) ->
    %%timer:sleep(20),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{pool_name = PoolName} = _State) ->
    ?YAWP_LOG_INFO("Worker ~p leaves pool: ~p", [self(), PoolName]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
