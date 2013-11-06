-module(child).
-export([metadata/0]).

metadata() ->
	[
		%% the inherit flag determines whether this resource's
		%% urls will be appended after a parent's url
		{inherit, true},

		%% define a root uri to prepend to all urls
		{root, "/child"},

		%% the name of the callback module that will handle
		%% all actions for this resource
		{module, child_server},

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
				{url, "/:child.id:"},

				%% the method for this action
				{method, get},

				%% which resource properties should be included as
				%% embedded resources with their own actions
				{embed, []}
			]}
		]}
	].