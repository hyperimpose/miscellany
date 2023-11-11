%%%-------------------------------------------------------------------
%%% @author drastik <derezzed@protonmail.com>
%%% @copyright (C) 2023 hyperimpose.org
%%% SPDX-License-Identifier: BSD-3-Clause
%%%
%%% Created: 1 Oct 2023 by drastik <derezzed@protonmail.com>
%%%-------------------------------------------------------------------

-module(bencode_tests).

-include_lib("eunit/include/eunit.hrl").


string_test_() ->
    [%% Normal string
     ?_assertEqual(bencode:encode(<<"spam">>), <<"4:spam">>),
     ?_assertEqual(bencode:decode(<<"4:spam">>), <<"spam">>),
     %% Non-ascii string
     ?_assertEqual(bencode:encode(<<"λ"/utf8>>), <<"2:λ"/utf8>>),
     ?_assertEqual(bencode:decode(<<"2:λ"/utf8>>), <<"λ"/utf8>>),
     ?_assertEqual(bencode:encode(<<"λ">>), <<"1:λ">>),
     ?_assertEqual(bencode:decode(<<"1:λ">>), <<"λ">>),
     %% Empty string
     ?_assertEqual(bencode:encode(<<"">>), <<"0:">>),
     ?_assertEqual(bencode:decode(<<"0:">>), <<"">>)].


integer_test_() ->
    [%% Normal integer
     ?_assertEqual(bencode:encode(3), <<"i3e">>),
     ?_assertEqual(bencode:decode(<<"i3e">>), 3),
     %% Negative integer
     ?_assertEqual(bencode:encode(-3), <<"i-3e">>),
     ?_assertEqual(bencode:decode(<<"i-3e">>), -3),
     %% Zero
     ?_assertEqual(bencode:encode(0), <<"i0e">>),
     ?_assertEqual(bencode:decode(<<"i0e">>), 0),
     %% Negative zero. i-0e is invalid. Decode it, but do not encode it.
     ?_assertEqual(bencode:encode(-0), <<"i0e">>),
     ?_assertEqual(bencode:decode(<<"i-0e">>), 0),
     %% Leading zero. i03e is invalid. Decode it, but do not encode it.
     ?_assertEqual(bencode:encode(03), <<"i3e">>),
     ?_assertEqual(bencode:decode(<<"i03e">>), 3),
     %% 65 bit unsigned integer (2^64 + 1)
     ?_assertEqual(bencode:encode(18_446_744_073_709_551_617), <<"i18446744073709551617e">>),
     ?_assertEqual(bencode:decode(<<"i18446744073709551617e">>), 18_446_744_073_709_551_617)].


list_test_() ->
    [%% Normal list
     ?_assertEqual(bencode:encode([<<"spam">>, <<"eggs">>]), <<"l4:spam4:eggse">>),
     ?_assertEqual(bencode:decode(<<"l4:spam4:eggse">>), [<<"spam">>, <<"eggs">>]),
     %% List of list
     ?_assertEqual(bencode:encode([[<<"spam">>]]), <<"ll4:spamee">>),
     ?_assertEqual(bencode:decode(<<"ll4:spamee">>), [[<<"spam">>]]),
     %% Empty list
     ?_assertEqual(bencode:encode([]), <<"le">>),
     ?_assertEqual(bencode:decode(<<"le">>), []),
     %% Empty list of empty list
     ?_assertEqual(bencode:encode([[]]), <<"llee">>),
     ?_assertEqual(bencode:decode(<<"llee">>), [[]]),
     %% Error: non terminated list
     ?_assertError(function_clause, bencode:decode(<<"l4:testg">>))
].


dictionary_test_() ->
    [%% String value
     ?_assertEqual(bencode:encode(#{<<"cow">> => <<"moo">>, <<"spam">> => <<"eggs">>}),
                   <<"d3:cow3:moo4:spam4:eggse">>),
     ?_assertEqual(bencode:decode(<<"d3:cow3:moo4:spam4:eggse">>),
                   #{<<"cow">> => <<"moo">>, <<"spam">> => <<"eggs">>}),
     %% List of strings value
     ?_assertEqual(bencode:encode(#{<<"spam">> => [<<"a">>, <<"b">>]}), <<"d4:spaml1:a1:bee">>),
     ?_assertEqual(bencode:decode(<<"d4:spaml1:a1:bee">>), #{<<"spam">> => [<<"a">>, <<"b">>]}),
     %% Multiple entries
     ?_assertEqual(bencode:encode(#{<<"publisher">> => <<"bob">>,
                                    <<"publisher-webpage">> => <<"www.example.com">>,
                                    <<"publisher.location">> => <<"home">>}),
                   <<"d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee">>),
     ?_assertEqual(bencode:decode(<<"d9:publisher3:bob17:publisher-webpage"
                                    "15:www.example.com18:publisher.location4:homee">>),
                   #{<<"publisher">> => <<"bob">>,
                     <<"publisher-webpage">> => <<"www.example.com">>,
                     <<"publisher.location">> => <<"home">>}),
    %% Empty dictionary
     ?_assertEqual(bencode:encode(#{}), <<"de">>),
     ?_assertEqual(bencode:decode(<<"de">>), #{})].
