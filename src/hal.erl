%%% @author Alex Robson
%%% @copyright 2013
%%% @doc
%%%
%%% 
%%% 
%%% @end
%%% Licensed under the MIT license - http://www.opensource.org/licenses/mit-license
%%% Created May 10, 2013 by Alex Robson

-module(hal).
-export([render/3]).

%% Renders the Model's hypermedia as the given Resource : Action
%% The rendered result is a proplist (abstract) that can be translated
%% to specific content types (JSON, XML).
-spec render(Resource::atom(), Action::atom(), Model::term()) -> [term()].
render(Resource, Action, Model) -> render(Resource, Action, Model, "").

%% No model means nothing to render, send back an empty response.
render(_Resource, _Action, undefined, _ParentUrl) -> [];
render(_Resource, _Action, [], _ParentUrl) -> [];

%% Generate hypermedia for a list of models
render(Resource, Action, [Model|Models], ParentUrl) when is_list(Model) ->
	[render_action(Resource, Action, Model, ParentUrl) | render(Resource, Action, Models, ParentUrl)];

%% Generate hypermedia for a single models
render(Resource, Action, Model, ParentUrl) ->
	render_action(Resource, Action, Model, ParentUrl).

%% Generate hypermedia for a specific action including the
%% links and embedded resources (if any)
render_action(Resource, Action, Model, _ActionFilter, ParentUrl) ->
	Root = meta:get_root_url(Resource),
	ActionMeta=meta:get_action(Resource, Action),
	ModelRepresentation = lists:reverse(render_model(ActionMeta, Model)),
	Links = render_links(Root, Model, Resource, ParentUrl),
	Primary = [{'_links', Links} | ModelRepresentation],
	Body = case render_embed(Root, Resource, ActionMeta, Model, ParentUrl) of
		undefined -> Primary;
		[] -> Primary;
		[Embedded] -> [{'_embedded', Embedded} | Primary]
	end,
	lists:reverse(Body).

%% Extract the filter to limit which actions are rendered
render_action(Resource, {Action,Filter}, Model, ParentUrl) ->
	render_action(Resource, Action, Model, Filter, ParentUrl);
render_action(Resource, Action, Model, ParentUrl) ->
	render_action(Resource, Action, Model, fun(_) -> true end, ParentUrl).

%% Render embedded resources
render_embed(Root, Resource, Action, Model, ParentUrl) ->
	render_embedded(Root, Resource, meta:get_embedded(Action), Model, ParentUrl).

%% Empty embedded resource produces an empty list
render_embedded(_, _, undefined, _, _) -> [];
render_embedded(_, _, [], _, _) -> [];

%% Render for every embedded resource
render_embedded(Root, Resource, [Embed|Embedded], Model, ParentUrl) ->
	[Name, EmbeddedResource, Action, _Actions] = meta:get_values([name,resource,action,actions], Embed),
	Equality = fun(A,B) -> A =:= B end,
	Filter = fun(A) -> lists:any( Equality, A ) end,
	Models = proplists:get_value( Name, Model ),
	Item = render( EmbeddedResource, {Action, Filter}, Models, Root ++ ParentUrl),
	[Item|render_embedded(Root, Resource, Embedded, Model, ParentUrl)].

%% Extract the action list to be rendered as links
render_links(Root, Model, Resource, ParentUrl) ->
	render_links(Root, Resource, meta:get_actions( Resource ), Model, ParentUrl ).

%% Once all action links are rendered return an empty list
render_links(_, _, [], _, _) -> [];

%% Construct links for actions specified by the resource
render_links(Root, Resource, [Action|Actions], Model, ParentUrl) ->
	[Name, Method, Inherits, UrlTemplate] = meta:get_values([name, method, inherit, url], Action),
	Url = url:create_url(UrlTemplate, Model, Resource),
	FullUrl = string:join([Root, ParentUrl, Url], ""),
	Option = Model =:= [],
	HasPlaceHolder = string:str( FullUrl, ":" ) > 0,
	Valid = (Option andalso not Inherits andalso not HasPlaceHolder ) orelse not Option,
	if
		Valid -> 
			Link = {Name, [{href, FullUrl}, {method, Method}]},
			[Link|render_links(Root, Resource, Actions, Model, ParentUrl)];
		true -> render_links(Resource, Actions, Model, ParentUrl)
	end.

%% Render the body of the response according to the properties
%% listed in the Action metadata
render_model(Action, Model) ->
	Props = case { proplists:get_value(properties, Action),
			proplists:get_value(map, Action) } of
		{Properties, undefined} ->
			[{K, proplists:get_value(K, Model)} || K <- Properties];
		{undefined, Map} ->
			Map(Model);
		_ -> []
	end,
	[X || {_,V}=X <- Props, V=/=undefined].