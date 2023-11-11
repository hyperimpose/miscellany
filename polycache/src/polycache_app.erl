%%%-------------------------------------------------------------------
%%% @author drastik <derezzed@protonmail.com>
%%% @copyright (C) 2023 hyperimpose.org
%%% SPDX-License-Identifier: BSD-3-Clause
%%%
%%% Created: 01 Oct 2023 by drastik <derezzed@protonmail.com>
%%%-------------------------------------------------------------------

-module(polycache_app).

-behaviour(application).


-export([start/2, stop/1]).


start(_Type, _Args) ->
    polycache_sup:start_link().

stop(_State) ->
    ok.
