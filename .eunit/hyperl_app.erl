%%% @author Alex Robson
%%% @copyright 2013
%%% @doc
%%%
%%% 
%%% 
%%% @end
%%% Licensed under the MIT license - http://www.opensource.org/licenses/mit-license
%%% Created May 10, 2013 by Alex Robson

-module(hyperl_app).

-behaviour(application).

%% Application callbacks
-export([start/1, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(Resources) ->
	start([], [Resources]).

start(_StartType, Args) ->
    hyperl_sup:start_link(Args).

stop(_State) ->
    ok.