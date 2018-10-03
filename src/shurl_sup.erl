%%%-------------------------------------------------------------------
%% @doc shurl top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(shurl_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  {ok, Pools} = application:get_env(shurl, pools),
  supervisor:start_link({local, ?SERVER}, ?MODULE, [Pools]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Pools]) ->
  PoolSpecs = pool(Pools),
  {ok, { {one_for_all, 0, 1}, PoolSpecs} }.

%%====================================================================
%% Internal functions
%%====================================================================

pool(Pools) ->
  lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
    PoolArgs = [{name, {local, Name}},
                {worker_module, shurl_worker}] ++ SizeArgs,
    poolboy:child_spec(Name, PoolArgs, WorkerArgs)
  end, Pools).
