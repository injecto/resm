-module(resm_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
  {ok, {#{}, [
    #{
      id => req_handler,
      start => {req_handler, start_link, []},
      modules => [req_handler]
    }
  ]}}.
