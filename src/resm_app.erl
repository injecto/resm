-module(resm_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/allocate/:user", http_handler, alloc},
      {"/deallocate/:resource", http_handler, dealloc},
      {"/list", http_handler, list},
      {"/list/:user", http_handler, user_list},
      {"/reset", http_handler, reset},
      {'_', http_handler, bad_req}
    ]}
  ]),
  {ok, Port} = application:get_env(resm, service_port),
  {ok, _} = cowboy:start_http(listener, 100, [{port, Port}],
    [{env, [{dispatch, Dispatch}]}]),

  {ok, _} = resm_sup:start_link().


stop(_State) ->
  ok.
