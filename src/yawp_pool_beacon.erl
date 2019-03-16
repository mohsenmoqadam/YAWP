-module(yawp_pool_beacon).

-author('MohsenMoqadam@yahoo.com').
-behaviour(gen_server).

%% API
-export([start_link/1,
         get_name/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         format_status/2]).

-include("yawp.hrl").
-define(SERVER, ?MODULE).
-define(PINGINTERVAL, 100).

-record(state, {pool_workers = [] :: list(pid()),
                timer             :: reference()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(#yawp_pool{name = Name}) ->
    gen_server:start_link({local, get_beacon_name(Name)},
                          ?MODULE,
                          [],
                          []).

-spec get_name(yawp_pool_name()) -> {ok, yawp_pool_beacon_name()}.
get_name(Name) ->
    {ok, get_beacon_name(Name)}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    timer:send_interval(?PINGINTERVAL, {send_beacon, beacon_service_feedback}),
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(#yawp_signal{name = worker_join_pool,
                         sender = Pid},
            _From,
            #state{pool_workers = OldPids} = State) ->
    Reply = ok,
    NewState = State#state{pool_workers = [Pid | OldPids]},
    {reply, Reply, NewState};
handle_call(#yawp_signal{name = worker_leave_pool,
                         sender = WorkerPid},
            _From,
            #state{pool_workers = OldPids} = State) ->
    Fun = fun(E) -> if E =:= WorkerPid -> false; true -> true end end,
    Reply = ok,
    NewState = State#state{pool_workers = lists:filter(Fun, OldPids)},
    {reply, Reply, NewState};
handle_call(get_pid, _From, State) ->
    Reply = {ok, self()},
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({send_beacon, beacon_service_feedback},
            #state{pool_workers = Pids} = State) ->
    Signal = #yawp_signal{name = beacon_service_feedback,
                          sender = self(),
                          receivers = Pids},
    yawp_utils:send_signal(Signal),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_beacon_name(PoolName) ->
    list_to_atom("yawp_pool_" ++ atom_to_list(PoolName) ++ "_beacon").
