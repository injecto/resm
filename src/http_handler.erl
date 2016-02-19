-module(http_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_, Req, alloc) ->
  {User, Req2} = cowboy_req:binding(user, Req),
  {ok, Req2, {alloc, User}};

init(_, Req, dealloc) ->
  {Res, Req2} = cowboy_req:binding(resource, Req),
  {ok, Req2, {dealloc, binary_to_integer(Res)}};

init(_, Req, user_list) ->
  {User, Req2} = cowboy_req:binding(user, Req),
  {ok, Req2, {user_list, User}};

init(_, Req, Opts) ->
  {ok, Req, Opts}.

handle(Req, {alloc, User} = State) ->
  case req_handler:allocate(User) of
    no_res ->
      {ok, Req2} = cowboy_req:reply(503, [], <<"Out of resources.">>, Req);
    Res ->
      {ok, Req2} = cowboy_req:reply(201, [], integer_to_binary(Res), Req)
  end,
  {ok, Req2, State};

handle(Req, {dealloc, Res} = State) ->
  case req_handler:deallocate(Res) of
    ok ->
      {ok, Req2} = cowboy_req:reply(204, Req);
    no_alloc ->
      {ok, Req2} = cowboy_req:reply(404, [], <<"Not allocated.">>, Req)
  end,
  {ok, Req2, State};

handle(Req, list = State) ->
  Pool = req_handler:list(),
  {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], jsx:encode(Pool), Req),
  {ok, Req2, State};

handle(Req, {user_list, User} = State) ->
  Rs = req_handler:list(User),
  {ok, Req2} = cowboy_req:reply(200, [], jsx:encode(Rs), Req),
  {ok, Req2, State};

handle(Req, reset = State) ->
  ok = req_handler:reset(),
  {ok, Req2} = cowboy_req:reply(204, Req),
  {ok, Req2, State};

handle(Req, bad_req = State) ->
  ok = req_handler:reset(),
  {ok, Req2} = cowboy_req:reply(400, [], <<"Bad request.">>, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.