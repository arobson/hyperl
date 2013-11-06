%%% @author Alex Robson
%%% @copyright 2013
%%% @doc
%%%
%%% Hypermedia middleware behavior
%%% 
%%% @end
%%% Licensed under the MIT license - http://www.opensource.org/licenses/mit-license
%%% Created May 10, 2013 by Alex Robson

-module(hal_middleware).
-behaviour(elli_handler).
-export([handle/2, handle_event/3]).
-compile(export_all).

%% -----------------------------------------------------------------------------
%%  elli_handler
%% -----------------------------------------------------------------------------

handle(Req, Args) ->
	Method = format_method(elli_request:method(Req)),
	Path = binary_to_list(elli_request:raw_path(Req)),
	Routes = filter_routes(Method, Path),
	case get_route_match(Path, Routes) of
		ignore -> 
			ignore;
		{Resource, Module, Action, PathArgs} ->
			{Code, Body} = Module:Action(Req, PathArgs ++ Args),
			Data = hal:render(Resource, Action, Body),
			Json = json:encode(Data, [pretty]),
			{Code, [], Json}
	end.

%% Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) -> ok.

%% -----------------------------------------------------------------------------
%%  Internal
%% -----------------------------------------------------------------------------

%% Retrieve the map from ETS first, then call to get matching routes
filter_routes(Method, Path) ->
	case ets:lookup(hyperl, route_map) of
		[] -> [];
		[{_,Map}] -> filter_routes(Method, Path, Map)
	end.

%% Use is_match to filter possible routes available in the map for the current request
filter_routes(Method, Path, Map) ->
	PathLength = length(Path),
	[X || {_, Verb, _, _, Root, _, _}=X <- Map, Verb =:= Method, X =/= ignore, is_match(Root, Path, PathLength)].

%% Check to see if the Root for a specific route matches the requested path
is_match(Root, Path, PathLength) -> 
	RootLength = length(Root),
	case PathLength >= RootLength of
		true -> Root =:= string:substr(Path, 1, length(Root));
		_ -> false
	end.

format_method(Method) -> list_to_atom(string:to_lower(atom_to_list(Method))).

%% Ignore favicon requests (for now)
get_route_match(<<"/favicon.ico">>, _) -> ignore;

%% Convert binary paths to list for comparisons
get_route_match(Path, Routes) when is_binary(Path) -> get_route_match(binary_to_list(Path), Routes);

%% Check possible routes against path using pre-calculated route patterns
get_route_match(Path, Routes) ->
	Matches = [get_path_match(Path, Route) || Route <- Routes],
	case [ Match || Match <- Matches, Match =/= ignore] of
		[First|_] -> First;
		_ -> ignore
	end.

%% Test a path against a Route's regular expression
get_path_match(Path, {Resource, _, Module, Action, _, RawUrl, Regex}) -> 
	case re:run(Path, Regex, [global, {capture, all, list}]) of
		{match, [[_|Values]]} -> 
			Keys = url:token_list(RawUrl),
			PathArgs = lists:zip(Keys,Values),
			{Resource, Module, Action, PathArgs};
		_ -> ignore
	end.