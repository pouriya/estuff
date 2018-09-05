%% -----------------------------------------------------------------------------
-module({{name}}_SUITE).
-author("{{author}}").
%% -----------------------------------------------------------------------------
%% Exports:

%% ct callbacks:
-export([init_per_suite/1
        ,end_per_suite/1
        ,all/0
        ,init_per_testcase/2
        ,end_per_testcase/2]).

-export(['1'/1
        ,'2'/1
        ,'3'/1
        ,'4'/1
        ,'5'/1
        ,'6'/1
        ,'7'/1
        ,'8'/1
        ,'9'/1
        ,'10'/1]).

%% -----------------------------------------------------------------------------
%% Records & Macros & Includes:

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% -----------------------------------------------------------------------------
%% ct callbacks:


all() ->
    IsTestCase = 
        fun
            ({Func, Arity}) when Arity == 1 ->
                try 
                    _ = erlang:list_to_integer(erlang:atom_to_list(Func)),
                    true
                catch
                    _:_ ->
                        false
                end;
            (_) ->
                false
        end,
    % '1', '2', ...
    [Func || {Func, _}=FuncArity <- ?MODULE:module_info(exports)
    ,IsTestCase(FuncArity)].


init_per_suite(Cfg) ->
    application:start(sasl),
    Cfg.


end_per_suite(Cfg) ->
    application:stop(sasl),
    Cfg.


init_per_testcase(_TestCase, Cfg) ->
    Cfg.


end_per_testcase(_TestCase, _Cfg) ->
    ok.

%% -----------------------------------------------------------------------------
%% Test cases:


'1'(Cfg) ->
    _ = Cfg,
    ok.


'2'(Cfg) ->
    _ = Cfg,
    ok.



'3'(Cfg) ->
    _ = Cfg,
    ok.



'4'(Cfg) ->
    _ = Cfg,
    ok.



'5'(Cfg) ->
    _ = Cfg,
    ok.



'6'(Cfg) ->
    _ = Cfg,
    ok.



'7'(Cfg) ->
    _ = Cfg,
    ok.



'8'(Cfg) ->
    _ = Cfg,
    ok.



'9'(Cfg) ->
    _ = Cfg,
    ok.



'10'(Cfg) ->
    _ = Cfg,
    ok.
