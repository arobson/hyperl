%%% @author Alex Robson
%%% @copyright 2013
%%% @doc
%%%
%%% A utility module to help with url construction
%%% 
%%% @end
%%% Licensed under the MIT license - http://www.opensource.org/licenses/mit-license
%%% Created May 10, 2013 by Alex Robson

-module(url).

-export([create_url/3, token_list/1]).

%% 
-spec create_url(list(), [], term()) -> list().
create_url(Url, Values, Resource) ->
	Tokens = get_tokens(Url),
	process_tokens(Tokens, Url, Values, Resource).

%% Provide a normalized list of tokens found in Url.
%% example URL: "/parent/:parent.id:/child/:child.id:""
%% result: ["parentId","childId"]
-spec token_list(list()) -> [list()].
token_list(Url) -> 
	[ list_to_atom(A ++ capitalize(B)) || {_, A, B, _}<-get_tokens(Url)].

%% Take a token tuple and create the camel-case version.
%% Tuples that include a resource will join it to a property
%% with the first letter capitalized.
camel_case({token, Property, _}) -> Property;
camel_case({token, Resource, Property, _}) ->
	string:join([Resource,capitalize(Property)],"").

%% Capitalize the first letter of the given string
capitalize(String) ->
	string:join([
		string:to_upper(
			string:substr(String,1,1)
		),
		string:substr(String,2)
	], "").

%% Find all tokens (place holders) in a URL.
%% The difference between plain and exact:
%% 		Plain =:= "resource.property"
%% 		Exact =:= ":resource.property:"
find_tokens(Url) ->
	case re:run(Url, "[:]([^:\/]*)[:]", [global, {capture, all, list}]) of
		nomatch -> [];
		{match, Tokens} -> [{Plain,Exact}||[Exact,Plain]<-Tokens]
	end.

%% Extract the OriginalFormat of the token from the tuple.
%% Original format essentially is the token wrapped with ':'
get_target({token, _, _, OriginalFormat}) -> OriginalFormat;
get_target({token, _, OriginalFormat}) -> OriginalFormat.

%% Take the list of tokens [{"resource.property",":resource.property:"}] 
%% and turn them into a structured tuple list (see parse_token for format)
get_tokens(Url) ->
	[parse_token(Token)||Token<-find_tokens(Url)].

%% Extract the property value specified by te token or return the 
%% token as a placeholder in the form ":resourceProperty"
get_token_replacement(Resource, Token, Values) when is_atom(Resource) ->
	get_token_replacement(atom_to_list(Resource), Token, Values);
get_token_replacement(Resource, Token, Values) ->
	case get_token_value(Resource, Token, Values) of
		undefined -> string:join([ ":", camel_case( Token ) ], "");
		Value -> Value
	end.

%% The proplist is empty, return undefined
get_token_value(_, _, []) ->
	undefined;

%% Read the value of the token from the resource/property
%% Assumes that the Resource is a proplist
get_token_value(Resource, {token, Resource, Property, OriginalFormat}, Values) ->
	get_token_value( Resource, {token, Property, OriginalFormat}, Values);
get_token_value(_Resource, {token, Property, _OriginalFormat}, Values) ->
	proplists:get_value(list_to_atom(Property), Values);

%% Attempt to extract the property from a nested proplist within resource.
get_token_value(_Resource, {token, TokenResource, Property, _OriginalFormat}, Values) ->
	case proplists:get_value(list_to_atom(TokenResource), Values) of
		undefined -> undefined;
		Nested -> proplists:get_value(list_to_atom(Property), Nested)
	end.

%% Break the token apart into Resource and Property and return a
%% tuple of the format {token, "resource", "property", ":resource.property:"}
parse_token({Token, OriginalFormat}) ->
	case string:tokens(Token, ".") of
		[Property] -> {token, Property, OriginalFormat};
		[Resource,Property] -> {token, Resource, Property, OriginalFormat};
		_ -> invalid_token
	end.

%% No tokens left; return Url
process_tokens([], Url, _, _) -> Url;

%% Empty values; no tokens to replace, return Url
process_tokens(_, Url, [], _) -> Url;

%% Replace Token with the corresponding property in Values for this Resource
process_tokens([Token|Tokens], Url, Values, Resource) ->
	Replacement = format(get_token_replacement(Resource, Token, Values)),
	Target = get_target(Token),
	NewUrl = re:replace(Url, Target, Replacement, [global, {return,list}]),
	process_tokens(Tokens, NewUrl, Values, Resource).

%% Ensures the replacement is in string format
format(X) when is_list(X) -> X;
format(X) -> io_lib:format("~p", [X]).