-module(req_handler).

-behaviour(gen_server).

-export([start_link/1, stop/1, start/1]).
-export([allocate/1, deallocate/1, list/0, list/1, reset/0]).
-export([init/1, handle_call/3, terminate/2, code_change/3, handle_info/2, handle_cast/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ResourceNum) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, ResourceNum, []).

start(ResourceNum) ->
  gen_server:start({local, ?SERVER}, ?MODULE, ResourceNum, []).

stop(ServerRef) ->
  gen_server:stop(ServerRef).

allocate(User) ->
  gen_server:call(?MODULE, {alloc, User}).

deallocate(Resource) ->
  gen_server:call(?MODULE, {free, Resource}).

list() ->
  gen_server:call(?MODULE, list).

list(User) ->
  gen_server:call(?MODULE, {list, User}).

reset() ->
  gen_server:call(?MODULE, reset).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init(ResourceNum) when ResourceNum >= 0 ->
  {ok, #{allocated => #{}, deallocated => populate(ResourceNum)}};

init(_) ->
  {stop, illegal_res_num}.

populate(ResourceNum) ->
  lists:seq(0, ResourceNum - 1).

handle_call({alloc, User}, _From, #{allocated := A, deallocated := [Res | T]} = Pool) ->
  {reply, Res, Pool#{allocated := A#{Res => User}, deallocated := T}};

handle_call({alloc, _}, _From, #{deallocated := []} = Pool) ->
  {reply, no_res, Pool};

handle_call({free, Res}, _From, #{allocated := A, deallocated := D} = Pool) ->
  case maps:is_key(Res, A) of
    true ->
      {reply, ok, Pool#{allocated := maps:remove(Res, A), deallocated := [Res | D]}};
    false ->
      {reply, no_alloc, Pool}
  end;

handle_call(list, _From, Pool) ->
  {reply, Pool, Pool};

% linear complexity: can become a bottleneck
handle_call({list, User}, _From, #{allocated := A} = Pool) ->
  AllocatedByUser = fun(_, V) -> V =:= User end,
  {reply, maps:keys(maps:filter(AllocatedByUser, A)), Pool};

handle_call(reset, _From, #{allocated := A, deallocated := D} = Pool) ->
  {reply, ok, Pool#{allocated := #{}, deallocated := lists:append(maps:keys(A), D)}}.

terminate(_Reason, #{allocated := A, deallocated := D}) ->
  lists:foreach(fun free/1, maps:keys(A)),
  lists:foreach(fun free/1, D),
  ok.

free(_Res) ->
  % nontrivial release
  ok.

handle_cast(_Req, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
