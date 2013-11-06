%%% @author Alex Robson
%%% @copyright 2013
%%% @doc
%%%
%%% 
%%% 
%%% @end
%%% Licensed under the MIT license - http://www.opensource.org/licenses/mit-license
%%% Created May 10, 2013 by Alex Robson

-module(hyperl_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

-define(SERVER, ?MODULE).

%%===================================================================
%%% API
%%===================================================================

start_link(Resources) ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, Resources).

init(Resources) ->
	RestartStrategy = one_for_one,
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 60,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Router = create_child_spec(route_server, worker, permanent, 1000, [Resources]),
    
    {ok, {SupFlags, [Router]}}.

%%===================================================================
%%% Internal functions
%%===================================================================

create_child_spec(Child, Type, Restart, Shutdown, Args) ->
    {Child, { Child, start_link, Args }, Restart, Shutdown, Type, [Child]}.