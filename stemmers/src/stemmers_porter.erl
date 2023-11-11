%%%-------------------------------------------------------------------
%%% @author drastik <derezzed@protonmail.com>
%%% @copyright (C) 2023 hyperimpose.org
%%% SPDX-License-Identifier: BSD-3-Clause
%%%
%%% Created: 3 Nov 2023 by drastik <derezzed@protonmail.com>
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @title The Porter Stemming Algorithm
%%%
%%% @doc
%%% The Porter stemming algorithm (or ‘Porter stemmer’) is a process
%%% for removing the commoner morphological and inflexional endings
%%% from words in English. Its main use is as part of a term
%%% normalisation process that is usually done when setting up
%%% Information Retrieval systems.
%%%
%%% @reference https://tartarus.org/martin/PorterStemmer/
%%% @reference https://tartarus.org/martin/PorterStemmer/def.txt
%%%
%%% A local version of the def.txt document from the second link can be
%%% found at /doc/internal/porter.txt
%%%
%%% This implementation differentiates from the published algorithm of
%%% document above. It includes the following two rules from the website:
%%%
%%% There is an extra rule in Step 2,
%%%     (m>0) logi -> log    So archaeology is equated with archaeological etc.
%%%
%%% The Step 2 rule
%%%     (m>0) abli -> able
%%% is replaced by
%%%     (m>0) bli -> ble     So possibly is equated with possible etc.
%%%
%%% The test cases from the site are also included and can be run with
%%% stemmers_porter:test().
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(stemmers_porter).

-include_lib("eunit/include/eunit.hrl").


-export([stem/1]).


%%%===================================================================
%%% API
%%%===================================================================

stem([_, _]  = W) -> W;  % do not stem 2 letter words
stem([_]     = W) -> W;  % do not stem 1 letter words
stem(Word) -> step_1a(lists:reverse(Word)).


%%%===================================================================
%%% Conditions
%%%===================================================================

%% A \consonant\ in a word is a letter other than A, E, I, O or U, and other
%% than Y preceded by a consonant

-define(IS_VOWEL(C, Allow_Y), (       C =:= $a
                               orelse C =:= $e
                               orelse C =:= $i
                               orelse C =:= $o
                               orelse C =:= $u
                               orelse (Allow_Y andalso C =:= $y))).

%% measure - m

m([]) -> 0;
m(In) -> m(lists:reverse(In), 0).

m([C | R], M) when ?IS_VOWEL(C, false) -> m_v(R, M);
m([_ | R], M)                          -> m_lc(R, M).

m_lc([C | R], M) when ?IS_VOWEL(C, true) -> m_v(R, M);
m_lc([_ | R], M)                         -> m_lc(R, M);
m_lc([], M)                              -> M.

m_v([C | R], M) when ?IS_VOWEL(C, false) -> m_v(R, M);
m_v([_ | R], M)                          -> m_c(R, M + 1);
m_v([],      M)                          -> M.

m_c([C | R], M) when ?IS_VOWEL(C, true) -> m_v(R, M);
m_c([_ | R], M)                         -> m_c(R, M);
m_c([],      M)                         -> M.

%% *v* - the stem contains a vowel

v(In) -> v1(lists:reverse(In)).

v1([C | _]) when ?IS_VOWEL(C, false) -> true;
v1([_ | R])                          -> v2(R);
v1([])                               -> false.

v2([C | _]) when ?IS_VOWEL(C, true) -> true;
v2([_ | R])                         -> v2(R);
v2([])                              -> false.

%% *o - the stem ends cvc, where the second c is not W, X or Y

o([C2, V, C1 | _]) when not ?IS_VOWEL(C1, false),
                        ?IS_VOWEL(V, true),
                        not ?IS_VOWEL(C2, true), C2 /= $w, C2 /= $x -> true;
o(_Else)                                                            -> false.


%%%===================================================================
%%% Steps
%%%===================================================================

step_1a("sess" ++ S) -> step_1b("ss" ++ S);
step_1a("sei"  ++ S) -> step_1b("i"  ++ S);
step_1a("ss"   ++ S) -> step_1b("ss" ++ S);
step_1a("s"    ++ S) -> step_1b(S);
step_1a(W)           -> step_1b(W).

step_1b("dee" ++ S = W) ->
    case m(S) > 0 of
        true  -> step_1c("ee" ++ S);
        false -> step_1c(W)
    end;
step_1b("gni" ++ S = W) ->
    case v(S) of
        true  -> step_1b_1(S);
        false -> step_1c(W)
    end;
step_1b("de" ++ S = W) ->
    case v(S) of
        true  -> step_1b_1(S);
        false -> step_1c(W)
    end;
step_1b(W) ->
    step_1c(W).


step_1b_1("ta" ++ S) -> step_1c("eta" ++ S);
step_1b_1("lb" ++ S) -> step_1c("elb" ++ S);
step_1b_1("zi" ++ S) -> step_1c("ezi" ++ S);
step_1b_1([C, C | S]) when not ?IS_VOWEL(C, false), C /= $l, C /= $s, C /= $z ->
    step_1c([C] ++ S);
step_1b_1(W) ->
    case m(W) == 1 andalso o(W) of
        true  -> step_1c("e" ++ W);
        false -> step_1c(W)
    end.

step_1c("y" ++ S = W) ->
    case v(S) of
        true  -> step_2("i" ++ S);
        false -> step_2(W)
    end;
step_1c(W) ->
    step_2(W).


step_2("lanoita" ++ S = W) -> step_2_1(W, S, "eta");
step_2("noitazi" ++ S = W) -> step_2_1(W, S, "ezi");
step_2("ssenevi" ++ S = W) -> step_2_1(W, S, "evi");
step_2("ssenluf" ++ S = W) -> step_2_1(W, S, "luf");
step_2("ssensuo" ++ S = W) -> step_2_1(W, S, "suo");
step_2("lanoit"  ++ S = W) -> step_2_1(W, S, "noit");
step_2("itilib"  ++ S = W) -> step_2_1(W, S, "elb");
step_2("iltne"   ++ S = W) -> step_2_1(W, S, "tne");
step_2("ilsuo"   ++ S = W) -> step_2_1(W, S, "suo");
step_2("noita"   ++ S = W) -> step_2_1(W, S, "eta");
step_2("msila"   ++ S = W) -> step_2_1(W, S, "la");
step_2("itila"   ++ S = W) -> step_2_1(W, S, "la");
step_2("itivi"   ++ S = W) -> step_2_1(W, S, "evi");
step_2("icne"    ++ S = W) -> step_2_1(W, S, "ecne");
step_2("icna"    ++ S = W) -> step_2_1(W, S, "ecna");
step_2("rezi"    ++ S = W) -> step_2_1(W, S, "ezi");
step_2("illa"    ++ S = W) -> step_2_1(W, S, "la");
step_2("rota"    ++ S = W) -> step_2_1(W, S, "eta");
step_2("igol"    ++ S = W) -> step_2_1(W, S, "gol");  % Extra rule
step_2("ilb"     ++ S = W) -> step_2_1(W, S, "elb");  % Replace abli -> able
step_2("ile"     ++ S = W) -> step_2_1(W, S, "e");
step_2(W)                  -> step_3(W).

step_2_1(W, S, S2) ->
    case m(S) > 0 of
        true  -> step_3(S2 ++ S);
        false -> step_3(W)
    end.


step_3("etaci" ++ S = W) -> step_3_1(W, S, "ci");
step_3("evita" ++ S = W) -> step_3_1(W, S, "");
step_3("ezila" ++ S = W) -> step_3_1(W, S, "la");
step_3("itici" ++ S = W) -> step_3_1(W, S, "ci");
step_3("laci"  ++ S = W) -> step_3_1(W, S, "ci");
step_3("ssen"  ++ S = W) -> step_3_1(W, S, "");
step_3("luf"   ++ S = W) -> step_3_1(W, S, "");
step_3(W)                -> step_4(W).

step_3_1(W, S, S2) ->
    case m(S) > 0 of
        true  -> step_4(S2 ++ S);
        false -> step_4(W)
    end.


step_4("tneme" ++ S = W) -> step_4_1(W, S);
step_4("ecna"  ++ S = W) -> step_4_1(W, S);
step_4("ecne"  ++ S = W) -> step_4_1(W, S);
step_4("elba"  ++ S = W) -> step_4_1(W, S);
step_4("elbi"  ++ S = W) -> step_4_1(W, S);
step_4("tnem"  ++ S = W) -> step_4_1(W, S);
step_4("tna"   ++ S = W) -> step_4_1(W, S);
step_4("tne"   ++ S = W) -> step_4_1(W, S);
step_4("noi"   ++ S = W) -> step_4_2(W, S);  % attention: it is 4_2
step_4("msi"   ++ S = W) -> step_4_1(W, S);
step_4("eta"   ++ S = W) -> step_4_1(W, S);
step_4("iti"   ++ S = W) -> step_4_1(W, S);
step_4("suo"   ++ S = W) -> step_4_1(W, S);
step_4("evi"   ++ S = W) -> step_4_1(W, S);
step_4("ezi"   ++ S = W) -> step_4_1(W, S);
step_4("la"    ++ S = W) -> step_4_1(W, S);
step_4("re"    ++ S = W) -> step_4_1(W, S);
step_4("ci"    ++ S = W) -> step_4_1(W, S);
step_4("uo"    ++ S = W) -> step_4_1(W, S);
step_4(W)                -> step_5a(W).

step_4_1(W, S) ->
    case m(S) > 1 of
        true  -> step_5a(S);
        false -> step_5a(W)
    end.

step_4_2(W, [C | _] = S) ->
    case m(S) > 1 andalso (C =:= $s orelse C =:= $t) of
        true  -> step_5a(S);
        false -> step_5a(W)
    end;
step_4_2(W, []) ->
    step_5a(W).


step_5a("e" ++ S = W) ->
    case m(S) of
        M when M > 1  ->
            step_5b(S);
        M when M == 1 ->
            case o(S) of
                true  -> step_5b(W);
                false -> step_5b(S)
            end;
        _Else ->
            step_5b(W)
    end;
step_5a(W) ->
    step_5b(W).


step_5b("ll" ++ S = W) ->
    case m(W) > 1 of
        true  -> lists:reverse("l" ++ S);
        false -> lists:reverse(W)
    end;
step_5b(W) ->
    lists:reverse(W).


%%%===================================================================
%%% Tests
%%%===================================================================

vocab_test_() ->
    lists:map(fun ({V, O}) -> ?_assertEqual(O, stemmers_porter:stem(V)) end,
              load_vocab_mappings()).


load_vocab_mappings() ->
    io:format("~p~n", [code:priv_dir(stemmers)]),
    {ok, VIo} = file:open([code:priv_dir(stemmers), "/test/porter/voc.txt"],
                            [read, raw, read_ahead]),
    {ok, OIo} = file:open([code:priv_dir(stemmers), "/test/porter/output.txt"],
                            [read, raw, read_ahead]),
    load_vocab_mappings1(VIo, OIo, []).

load_vocab_mappings1(VIo, OIo, Acc) ->
    case {file:read_line(VIo), file:read_line(OIo)} of
        {eof, _}           -> lists:reverse(Acc);
        {_, eof}           -> lists:reverse(Acc);
        {{ok, V}, {ok, O}} ->
            V1 = string:trim(V),
            O1 = string:trim(O),
            load_vocab_mappings1(VIo, OIo, [{V1, O1} | Acc])
    end.
