-module(sws).
-compile(export_all).

start() ->
	start(8877).

start(Port) ->

	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy),

	N_acceptors = 10,

	Dispatch = cowboy_router:compile([
		%% {URIHost, list({URIPath, Handler, Opts})}
		{'_', [{'_', simple_web_server, []}]}
		]),

	cowboy:start_http(
		my_simple_web_server,
		N_acceptors,
		[{port, Port}],
		[{env, [{dispatch, Dispatch}]}]
	).