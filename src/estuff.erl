%%% ----------------------------------------------------------------------------
%%% @author <{{author}}>
%%% @doc
%%%         {{description}}
%%% @end

%% -----------------------------------------------------------------------------
-module({{name}}).
-author('{{author}}').
%% -----------------------------------------------------------------------------
%% Exports:

%% API:
-export([start/0
        ,stop/0]).

%% -----------------------------------------------------------------------------
%% API:

-spec
start() ->
    ok | {error, term()}.
%% @doc
%%     Starts {{name}} application.
%% @end
start() ->
    case application:ensure_all_started({{name}}) of
        {ok, _} ->
            ok;
        Err ->
            Err
    end.


-spec
stop() ->
    ok.
%% @doc
%%     Stops {{name}} application.
%% @end
stop() ->
    application:stop({{name}}).
