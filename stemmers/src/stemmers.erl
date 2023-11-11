%%%-------------------------------------------------------------------
%%% @author drastik <derezzed@protonmail.com>
%%% @copyright (C) 2023 hyperimpose.org
%%% SPDX-License-Identifier: BSD-3-Clause
%%%
%%% Created: 4 Nov 2023 by drastik <derezzed@protonmail.com>
%%%-------------------------------------------------------------------

-module(stemmers).

-export([porter/1]).


porter(Word) -> stemmers_porter:stem(unicode:characters_to_list(Word)).
