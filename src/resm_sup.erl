-module(resm_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
  {ok, ResourceNum} = application:get_env(resm, resource_num),
  {ok, {#{}, [
    #{
      id => req_handler,
      start => {req_handler, start_link, [ResourceNum]},
      modules => [req_handler]
    }
  ]}}.
