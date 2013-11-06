%%% @author Alex Robson
%%% @copyright 2013
%%% @doc
%%%
%%% Provide access to and processing of resource metadata
%%% 
%%% @end
%%% Licensed under the MIT license - http://www.opensource.org/licenses/mit-license
%%% Created May 10, 2013 by Alex Robson

-module(meta).

-export([
		get_action/2,
		get_actions/1,
		get_actions/2,
		get_embedded/1,
		get_metadata/1,
		get_values/2,
		get_values/3,
		get_root_url/1,
		get_routes/1
	]).

%% -----------------------------------------------------------------------------
%%  API
%% -----------------------------------------------------------------------------

%% Retrieves Action metadata from Resource
%% format:	[
%%				{inherit, [inherited properties proplist]},
%%				{name, Action},
%%				{properties, [id, title, description]},
%%				{url, ""},
%%				{method, atom()},
%%				{embed, []}
%%			]
-spec get_action(Resource::atom(), Action::atom()) -> [tuple()].
get_action(Resource, Action) ->
	[Target] = get_actions(Resource, fun({X,_}) -> X=:=Action end),
	Target.

%% Retrieves all actions from Resource
-spec get_actions(Resource::atom()) -> [[tuple()]].
get_actions(Resource) ->
	get_actions(Resource, fun(_) -> true end).

%% Retreives all actions from resource matching the filter.
%% Flattens the action tuple and prepends inherited properties
%% for convenience.
-spec get_actions(Resource::atom(), Filter::fun((tuple()) -> true | false)) -> [[tuple()]].
get_actions(Resource, Filter) ->
	Metadata = get_metadata(Resource),
	Inherits = proplists:get_value(inherit, Metadata, false),
	Actions = proplists:get_value(actions, Metadata),
	Flattened = [ [{name,Action}|Props] || {Action,Props}=X <- Actions, Filter(X)],
	[[{inherit, get_inherited(Inherits,Props)}|Props] || Props <- Flattened].

%% Retrieves all embedded resources for the given action metadata.
%% Flattens the embedded tuple for convenience.
-spec get_embedded(Action::[tuple()]) -> [tuple()].
get_embedded(Action) ->
	Embedded = proplists:get_value(embed, Action, []),
	[[{name,Name}|Props] || {Name, Props} <- Embedded].

%% Determines inheritance from another resource.
-spec get_inherited(Resource::atom(), Props::[tuple()]) -> true | false.
get_inherited(Resource, Props) ->
	case proplists:get_value(inherit, Props) of
		undefined -> Resource;
		X -> X
	end.

%% Retrieves the metadata for a specific resource (or list of resources).
%% Allows passing a string instead of an atom.
-spec get_metadata(Resources::[atom()]) -> [[{tuple}]]
				; (Resources::list()) -> [{tuple}]
				; (Resources::atom()) -> [{tuple}].
get_metadata([H|_]=Resources) when is_atom(H) -> [get_metadata(X) || X <- Resources];
get_metadata(Resource) when is_list(Resource) -> get_metadata(list_to_atom(Resource));
get_metadata(Resource) -> Resource:metadata().

%% Gets the root url for the resource (if one exists).
-spec get_root_url(Resource::atom()) -> list().
get_root_url(Resource) ->
	proplists:get_value(root, get_metadata(Resource), "").

%% Constructs metadata around the routes defined by the resource actions.
%% Format for a resulting route:
%% 	{
%%		Resource::atom(),
%%		Method::atom(),		%% HTTP method - get| put | post | ...
%%		Module::atom(), 	%% callback module for the resource 
%%		Action::atom(), 	%% action name
%%		Root::list(),		%% Root URL
%%		RawUrl::list()		%% Full URL pattern for the action
%%		Regex::list()		%% Regex pattern to indicate route match
%% 	}
%% A route will be created for every action and every resource in
%% Resources.
-spec get_routes(Resources::[atom()]) -> [tuple()]
				;(Resource::atom()) -> [tuple()].
get_routes(Resources) when is_list(Resources) -> 
	lists:flatten([ get_routes(Resource) || Resource <- Resources ]);
get_routes(Resource) ->
	Metadata = get_metadata(Resource),
	[Root, Module, Actions] = get_values([{root, ""}, module, actions], Metadata),
	[get_route_with_pattern(Resource, Module, Action, Root) || Action <- Actions].

%% -----------------------------------------------------------------------------
%%  Internal
%% -----------------------------------------------------------------------------

%% Add regex for route to the route tuple.
%% Add regex for route to the route tuple.
get_route_with_pattern(_, undefined, _, _) -> ignore;
get_route_with_pattern(Resource, Module, {Action, ActionProps}, Root) ->
	[Method, Url] = get_values([method, url], ActionProps),
	Combined = Root ++ Url,
	Regex = create_url_pattern(Combined) ++ ["$"],
	{Resource, Method, Module, Action, Root, Combined, Regex}.

%% Retrieve a list of keys from a proplist
%% Key can either be an atom or a tuple of {Key, DefaultValue}
get_values(Keys, List) -> get_values(Keys, List, []).

get_values([], [], Values) -> lists:reverse(Values);
get_values(_, [], Values) -> lists:reverse(Values);
get_values([], _ , Values) -> lists:reverse(Values);
get_values([{Key, Default}|Keys], List, Values) ->
	get_values(Keys, List, [proplists:get_value(Key, List, Default)|Values]);
get_values([Key|Keys], List, Values) ->
	get_values(Keys, List, [proplists:get_value(Key, List)|Values]).


%% Creates the regular expression for a given url pattern by
%% replacing tokens with ([^/]*) so that path variables can be
%% extracted via capture group while checking for route match.
create_url_pattern(List) when is_list(List) -> create_url_pattern(list_to_binary(List), "");
create_url_pattern(<<"">>) -> "".

create_url_pattern(<<"">>, Acc) -> Acc;
create_url_pattern(<<$/, $:, T/bitstring>>, Acc) -> 
	Stripped = strip_variable(T),
	create_url_pattern(Stripped, Acc ++ "[\/]([^\/]+)");
create_url_pattern(<<$:, T/bitstring>>, Acc) -> create_url_pattern(T, Acc);
create_url_pattern(<<$/, T/bitstring>>, Acc) -> create_url_pattern(T, Acc ++ "[\/]");
create_url_pattern(<<H, T/bitstring>>, Acc) -> create_url_pattern(T, Acc ++ [H]).

%% Remove the ':' token header from the URL and return the remainder.
strip_variable(<<$:, T/bitstring>>) -> T;
strip_variable(<<_, T/bitstring>>) -> strip_variable(T).