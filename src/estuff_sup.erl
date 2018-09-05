%%% ----------------------------------------------------------------------------
%%% @author <{{author}}>
%%% @doc
%%%         {{name}} root supervisor implementation.
%%% @end
%%% @hidden

%% -----------------------------------------------------------------------------
-module({{name}}_sup).
-author("{{author}}").
-behaviour(supervisor).
%% -----------------------------------------------------------------------------
%% Exports:

%% API:
-export([start_link/0]).

%% 'supervisor' callback:
-export([init/1]).

%% -----------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(PROC, ?MODULE).

%% -----------------------------------------------------------------------------
%% API:

-spec
start_link() ->
    {ok, pid()} | {error, term()} | ignore.
start_link() ->
    supervisor:start_link({local, ?PROC}, ?MODULE, undefined).


%% -----------------------------------------------------------------------------
%% 'supervisor' callbacks:

%% @hidden
init(_) -> % (undefined)
    {ok, { {one_for_all, 0, 1}, []} }.
