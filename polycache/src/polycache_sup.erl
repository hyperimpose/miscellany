%%%-------------------------------------------------------------------
%%% @author drastik <derezzed@protonmail.com>
%%% @copyright (C) 2023 hyperimpose.org
%%% SPDX-License-Identifier: BSD-3-Clause
%%%
%%% Created: 01 Oct 2023 by drastik <derezzed@protonmail.com>
%%%-------------------------------------------------------------------

-module(polycache_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Macros
-define(NAME, ?MODULE).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%--------------------------------------------------------------------

-spec start_link() -> {ok, Pid :: pid()} |
          {error, {already_started, Pid :: pid()}} |
          {error, {shutdown, term()}} |
          {error, term()} |
          ignore.

start_link() ->
    supervisor:start_link({local, ?NAME}, ?MODULE, []).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec init(Args :: term()) ->
          {ok, {SupFlags :: supervisor:sup_flags(),
                [ChildSpec :: supervisor:child_spec()]}}.

init(_Args) ->
    SupFlags = #{strategy  => one_for_one,
                 intensity => 1,
                 period    => 5},

    Polycache = #{id       => polycache,
                  start    => {polycache, start_link, []},
                  restart  => permanent,
                  shutdown => 5000,
                  type     => worker,
                  modules  => [polycache]},

    {ok, {SupFlags, [Polycache]}}.
