%%% @author Alex Robson
%%% @copyright 2013
%%% @doc
%%%
%%% Hypermedia middleware behavior
%%% 
%%% @end
%%% Licensed under the MIT license - http://www.opensource.org/licenses/mit-license
%%% Created May 10, 2013 by Alex Robson

-module(route_server).
-behavior(gen_server).
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-record(state, {}).

%%===================================================================
%%% API
%%===================================================================
get_route(Method, Path) when is_binary(Path) -> get_route(Method, binary_to_list(Path));
get_route(Method, Path) ->
	gen_server:call(?MODULE, {match, Method, Path}).

%%===================================================================
%%% gen_server
%%===================================================================

start_link(Resources) ->
	gen_server:start_link(?MODULE, [Resources], []).

init([Resources]) ->
	ets:new(hyperl, [ordered_set, protected, named_table, {read_concurrency, true}]),
	ets:insert(hyperl, {route_map, meta:get_routes(Resources)}),
	register(?MODULE, self()),
	{ok, #state{}}.

handle_call(state, _From, State) ->
	{reply, State, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%===================================================================
%%% Internal
%%===================================================================

filter_routes(Method, Path, Map) ->
	PathLength = length(Path),
	[X || {_, Verb, _, _, Root, _, _}=X <- Map, Verb =:= Method, X =/= ignore, is_match(Root, Path, PathLength)].

is_match(Root, Path, PathLength) -> 
	RootLength = length(Root),
	case PathLength >= RootLength of
		true -> Root =:= string:substr(Path, 1, length(Root));
		_ -> false
	end.