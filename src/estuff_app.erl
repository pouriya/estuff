%%% ----------------------------------------------------------------------------
%%% @author <{{author}}>
%%% @doc
%%%         {{name}} application behaviour implementation.
%%% @end
%%% @hidden

%% -----------------------------------------------------------------------------
-module({{name}}_app).
-author('{{author}}').
-behaviour(application).
%% -----------------------------------------------------------------------------
%% Exports:

%% 'application' callbacks:
-export([start/2
        ,stop/1]).

%% -----------------------------------------------------------------------------
%% 'application' callbacks:

%% @hidden
start(_, _) -> % (Type, InitArg)
    {{name}}_sup:start_link().


%% @hidden
stop(_) -> % (State)
    ok.
