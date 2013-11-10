# Hyperl
Hypermedia API middleware for Elli. This is proof-of-concept quality. It's bug ridden and everything is very much subject to change. I hope you'll weigh in if you have opinions.

## Concept
HTTP middleware should isolate HTTP implementation details from the application logic. Elli has excellent middleware support that should allow for all HTTP concerns to be handled without resource processes having to provide specialized responses.

This particular approach expects you to provide metadata that defines how to render a resource, its actions and any embedded children. 

This middleware has a ways to go. See the To Do section for more details.

## Including In Your Project
Add the following to {deps, []} in your rebar.config:
```erlang
{hyperl, "",
	{git, "git://github.com/arobson/hyperl",
	{branch, "master"} } }
```

## Adding To Your Supervisor
At the moment, Hyperl includes a gen_server that owns the ETS table where route maps are stored. This is done partly to prevent constant re-calculation of routes for every request and also to put ETS ownership someplace stable. In the future there will probably be calls added to the route_server to make it worth while.

```erlang
MetadataModules = [parent], %% the list of metadata resources
Hyperl = {hyperl_sup,
			{hyperl_sup, start_link, [MetadataModules]}, 
			permanent, 1000, supervisor, [hyperl_sup]
		}.
```

## Wiring Into Elli
The following block demonstrates the entire setup (including the previous supervisor setup).
```erlang
init([]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 3,
	MaxSecondsBetweenRestarts = 60,
	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

	MWConfig = [{mods, [
		{hal_middleware, []}
	]}],
	ElliOpts = [{callback, elli_middleware}, {port, 8090}, {callback_args, MWConfig}],
	Elli = {elli,
		{elli, start_link, [ElliOpts]},
		permanent, 1000, worker, [elli]
	}

	MetadataModules = [parent], %% the list of metadata resources
	Hyperl = {hyperl_sup,
		{hyperl_sup, start_link, [MetadataModules]}, 
		permanent, 1000, supervisor, [hyperl_sup]
	},
	{ok, {SupFlags, [Elli, Hyperl]}}.
```

## Example Resource Action
The resource callback module should provide a function to handle every action specified in the metada. The Data parameter is the body of the request (or [] if no body is included) and the Arguments is a proplist of the path and query string parameters. The key name for a path argument has the punctuation removed from the template and camel cases the second part. (example: :parent.id: -> parentId)

```erlang
action(Data, Arguments) -> {ResposeCode, Response}.
```

For now, the primary implementation detail that leaks into the actions is the need to include a response code.

## Defining Resource Metadata
Each resource should provide metadata as a seperate module. The following example demonstrates both the 

```erlang
-module(resource_type).
-export([metadata/0]).

metadata() ->
	[
		%% the inherit flag determines whether this resource's
		%% urls will be appended after a parent's url
		{inherit, false},

		%% define a root uri to prepend to all urls
		{root, "/api/parent"},

		%% the name of the callback module that will handle
		%% all actions for this resource
		{module, parent_server},

		{actions, [

			%% this action name will be the name of the link
			%% and the name of the function on the resource module
			{self, [

				%% controls which properties of the resource will
				%% be included in the response
				{properties, [id, title, description]},

				%% the url for this action.
				%% notice that path variables must be surrounded by ':'
				%% each variable should be two part, seperated by '.'
				%% the first part should be a resource and the second
				%% should be the property of the resource.
				{url, "/:parent.id:"},

				%% the method for this action
				{method, get},

				%% which resource properties should be included as
				%% embedded resources with their own actions
				{embed, [

					%% the property name of the resource
					{children, [

						%% the metadata module for the embedded resource
						{resource, child},

						%% the action to render for this resource
						{action, self},

						%% limits the number of embedded items to render
						{limit, 20},

						%% limits the number of actions to include (if any)
						{actions, [self]}
					]}
				]}
			]}
		]}
	].
```

## To Do
 * Add support for template only hypermedia to generate slimmer responses
 * Fix defect in method filters on embedded resources
 * Add roles so that actions can be filtered based on caller role