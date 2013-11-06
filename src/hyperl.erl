%%% @author Alex Robson
%%% @copyright 2013
%%% @doc
%%%
%%% 
%%% 
%%% @end
%%% Licensed under the MIT license - http://www.opensource.org/licenses/mit-license
%%% Created May 10, 2013 by Alex Robson

-module(hyperl).

-export([start/0, stop/0]).


%%===================================================================
%%% API
%%===================================================================

start() ->
	application:load(hyperl),
	application:start(hyperl).

stop() ->
	application:stop(hyperl).