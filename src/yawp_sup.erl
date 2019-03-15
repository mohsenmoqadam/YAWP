-module(yawp_sup).
-author('MohsenMoqadam@yahoo.com').
-behaviour(supervisor).

%% API
-export([start_link/0,
         new_pool/1]).

%% Supervisor callbacks
-export([init/1]).

-include("yawp.hrl").
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

new_pool(#yawp_pool{name = Name} = Pool) ->
    SupName = sup_name(Name),

    ChildSpec = {SupName,
                 {yawp_pool_sup, start_link, [Pool]},
                 transient,
                 100,
                 supervisor,
                 [yawp_pool_sup]},

    supervisor:start_child(?MODULE, ChildSpec).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    {ok, {{one_for_one, 1, 1}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
sup_name(PoolName) ->
    list_to_atom("yawp_pool_" ++ atom_to_list(PoolName) ++ "_sup").
