-module(middleware_tests).
-include_lib("eunit/include/eunit.hrl").

handle_test() ->
	Pid = route_server:start_link([parent, child, malformed]),
	meck:new(elli_request),
	meck:new(parent_server),
	test_correct_dispatch(),
	meck:unload(parent_server),
	meck:unload(elli_request).

test_correct_dispatch() ->
	meck:expect(elli_request, raw_path, 1, <<"/api/parent/1">>),
	meck:expect(elli_request, method, 1, 'GET'),
	meck:expect(parent_server, self, 2, {200, 
		[{id, "hi"}, {title, "test"}, {description, "this is a test"},
		 	{children, [
		 		[{id, 101},{title,"child 1"},{description,"child record"}],
		 		[{id, 102},{title,"child 2"},{description,"child record"}] 
		 	]}
		]}),
	Expected = <<"{\n  \"id\" : \"hi\",\n  \"title\" : \"test\",\n  \"description\" : \"this is a test\",\n  \"_links\" : {\n    \"self\" : {\n      \"href\" : \"/api/parent/hi\",\n      \"method\" : \"get\"\n    }\n  },\n  \"_embedded\" : [\n    {\n      \"id\" : 101,\n      \"title\" : \"child 1\",\n      \"description\" : \"child record\",\n      \"_links\" : {\n        \"self\" : {\n          \"href\" : \"/child/api/parent/101\",\n          \"method\" : \"get\"\n        }\n      }\n    },\n    {\n      \"id\" : 102,\n      \"title\" : \"child 2\",\n      \"description\" : \"child record\",\n      \"_links\" : {\n        \"self\" : {\n          \"href\" : \"/child/api/parent/102\",\n          \"method\" : \"get\"\n        }\n      }\n    }\n  ]\n}">>,
	Result = hal_middleware:handle({}, [{}]),
	?assertEqual({200,[], Expected}, Result).

handle_malformed_test() ->
	meck:new(elli_request),
	test_malformed_dispatch(),
	meck:unload(elli_request).

test_malformed_dispatch() ->
	meck:expect(elli_request, raw_path, 1, <<"/api/malformed/1">>),
	meck:expect(elli_request, method, 1, 'GET'),
	Result = hal_middleware:handle({}, [{}]),
	?assertEqual(ignore, Result).