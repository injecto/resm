-module(req_handler_test).

-include_lib("eunit/include/eunit.hrl").

empty_pool_creation_test() ->
  {ok, P} = req_handler:start(0),
  ok = req_handler:stop(P).

success_pool_creation_test() ->
  {ok, P} = req_handler:start(1),
  ok = req_handler:stop(P).

failed_pool_creation_test() ->
  ?assertEqual({error, illegal_res_num}, req_handler:start(-1)).

alloc_test_() ->
  {
    setup,
    fun() -> {ok, P} = req_handler:start(2), P end,
    fun req_handler:stop/1,
    [
      {"Allocation",
        ?_assertNotMatch(no_res, req_handler:allocate(<<"Konstantin">>))},
      {"Allocation",
        ?_assertNotMatch(no_res, req_handler:allocate(<<"Konstantin">>))},
      {"No resources for alloc",
        ?_assertEqual(no_res, req_handler:allocate(<<"Konstantin">>))},
      {"No resources for alloc",
        ?_assertEqual(no_res, req_handler:allocate(<<"Semen">>))},
      {"No resources for alloc",
        ?_assertEqual(no_res, req_handler:allocate(<<"Konstantin">>))}
    ]
  }.

dealloc_test_() ->
  {
    setup,
    fun() ->
      {ok, P} = req_handler:start(3),
      Res1 = req_handler:allocate(<<"Eduard">>),
      Res2 = req_handler:allocate(<<"Eduard">>),
      Res3 = req_handler:allocate(<<"Janna">>),
      {P, [Res1, Res2, Res3]}
    end,
    fun({P, _}) -> req_handler:stop(P) end,
    fun ({_, [R1, R2, R3]}) -> [
      {"Dealloc of unknown",
        ?_assertEqual(no_alloc, req_handler:deallocate(unknown))},
      {"Dealloc",
        ?_assertEqual(ok, req_handler:deallocate(R1))},
      {"Dealloc of non allocated",
        ?_assertEqual(no_alloc, req_handler:deallocate(R1))},
      {"Dealloc",
        ?_assertEqual(ok, req_handler:deallocate(R2))},
      {"Dealloc",
        ?_assertEqual(ok, req_handler:deallocate(R3))}
    ] end
  }.

list_test_() ->
  {
    setup,
    fun() -> {ok, P} = req_handler:start(2), P end,
    fun req_handler:stop/1,
    [
      {"All free",
        ?_assertMatch(#{allocated := #{}, deallocated := [_, _]}, req_handler:list())},
      fun() ->
        Res = req_handler:allocate(<<"Vasiliy">>),
        ?assertMatch(#{allocated := #{Res := <<"Vasiliy">>}, deallocated := [_]}, req_handler:list())
      end,
      {"No free", fun() ->
        Res = req_handler:allocate(<<"Mitrophan">>),
        ?assertMatch(#{allocated := #{Res := <<"Mitrophan">>}, deallocated := []}, req_handler:list())
      end}
    ]
  }.

user_list_test_() ->
  {
    setup,
    fun() -> {ok, P} = req_handler:start(3), P end,
    fun req_handler:stop/1,
    [
      {"All free",
        ?_assertMatch([], req_handler:list(<<"Dormidont">>))},
      fun() ->
        Res = req_handler:allocate(<<"Dormidont">>),
        ?assertMatch([Res], req_handler:list(<<"Dormidont">>))
      end,
      fun() ->
        Res1 = req_handler:allocate(<<"Rodion">>),
        Res2 = req_handler:allocate(<<"Rodion">>),
        ?assertMatch([Res1, Res2], req_handler:list(<<"Rodion">>))
      end,
      {"Unknown user",
        ?_assertMatch([], req_handler:list(<<"Laripront">>))}
    ]
  }.

reset_test_() ->
  {
    setup,
    fun() -> {ok, P} = req_handler:start(2), P end,
    fun req_handler:stop/1,
    [
      fun() ->
        ?assertEqual(ok, req_handler:reset()),
        ?assertMatch(#{allocated := #{}, deallocated := [_, _]}, req_handler:list())
      end,
      fun() ->
        Res1 = req_handler:allocate(<<"Klavdiya">>),
        Res2 = req_handler:allocate(<<"Klavdiya">>),
        ?assertMatch([Res1, Res2], req_handler:list(<<"Klavdiya">>)),
        ?assertEqual(ok, req_handler:reset()),
        ?assertMatch(#{allocated := #{}, deallocated := [_, _]}, req_handler:list())
      end
    ]
  }.
