%%%-------------------------------------------------------------------
%%% @author drastik <derezzed@protonmail.com>
%%% @copyright (C) 2023 hyperimpose.org
%%% SPDX-License-Identifier: BSD-3-Clause
%%%
%%% Created: 1 Oct 2023 by drastik <derezzed@protonmail.com>
%%%-------------------------------------------------------------------

-module(bencode).
-compile([bin_opt_info,
          {inline, [d/2, d_list/2, d_dict/2, d_int/3, d_str/3, d_next/3,
                    e/1, e_map/1]}]).

%% API
-export([decode/1, encode/1]).


%%====================================================================
%% API
%%====================================================================

decode(Input) -> d(iolist_to_binary(Input), []).

encode(Input) -> iolist_to_binary(e(Input)).

%%====================================================================
%% Decoding
%%====================================================================

d(<<$l, R/binary>>, Acc) -> d_list(R, [list, [] | Acc]);
d(<<$d, R/binary>>, Acc) -> d_dict(R, [#{} | Acc]);
d(<<$i, R/binary>>, Acc) -> d_int(R, Acc, []);
d(<<R/binary>>,    Acc)  -> d_str(R, Acc, []).

d_list(<<$e, R/binary>>, [list, L | Acc]) -> d_next(R, Acc, lists:reverse(L));
d_list(<<R/binary>>,     Acc)             -> d(R, Acc).

d_dict(<<$e, R/binary>>, [D | Acc]) -> d_next(R, Acc, D);
d_dict(<<R/binary>>,     Acc)       -> d(R, [dict_key | Acc]).

d_int(<<$e, R/binary>>, Acc, IntAcc) -> d_next(R, Acc, list_to_integer(lists:reverse(IntAcc)));
d_int(<<C, R/binary>>,  Acc, IntAcc) -> d_int(R, Acc, [C | IntAcc]).

d_str(<<$:, R/binary>>, Acc, LenAcc) ->
    Len = list_to_integer(lists:reverse(LenAcc)),
    <<Str:Len/binary, R1/binary>> = R,
    d_next(R1, Acc, Str);
d_str(<<C, R/binary>>, Acc,  LenAcc) ->
    d_str(R, Acc, [C | LenAcc]).

d_next(<<R/binary>>, [list, List | Acc],          Val) -> d_list(R, [list, [Val | List] | Acc]);
d_next(<<R/binary>>, [dict_key | Acc],            Key) -> d(R, [dict_val, Key | Acc]);
d_next(<<R/binary>>, [dict_val, Key, Dict | Acc], Val) -> d_dict(R, [Dict#{Key => Val} | Acc]);
d_next(<<>>,         [],                          Val) -> Val.


%%====================================================================
%% Encoding
%%====================================================================

e(R) when is_integer(R) -> [$i, integer_to_binary(R), $e];
e(R) when is_binary(R)  -> [integer_to_binary(size(R)), $:, R];
e(R) when is_list(R)    -> [$l, [e(X) || X <- R], $e];
e(R) when is_map(R)     -> [$d, e_map(lists:keysort(1, maps:to_list(R))), $e].

e_map([{K, V} | R]) when is_binary(K) ->
    [e(K), e(V) | e_map(R)];
e_map([]) ->
    [].
