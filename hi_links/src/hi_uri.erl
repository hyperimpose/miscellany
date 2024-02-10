%%%-------------------------------------------------------------------
%%% @author drastik <derezzed@protonmail.com>
%%% @copyright (C) 2024 hyperimpose.org
%%% SPDX-License-Identifier: BSD-3-Clause
%%%
%%% Created: 6 May 2023 by drastik <derezzed@protonmail.com>
%%%-------------------------------------------------------------------

-module(hi_uri).

-export([find/1]).


-define(LC(C), C >= $a andalso C =< $z).
-define(UC(C), C >= $A andalso C =< $Z).
-define(ALPHA(C), ?UC(C) orelse ?LC(C)).

-define(DIGIT(C), C >= $0 andalso C =< $9).
-define(HEXDIG(C),
        ?DIGIT(C)
        orelse (C >= $A andalso C =< $F)
        orelse (C >= $a andalso C =< $f)).

-define(gen_delims(C), C =:= $:; C =:= $/; C =:= $?; C =:= $#; C =:= $[;
                       C =:= $]; C =:= $@).
-define(sub_delims(C), C =:= $!; C =:= $$; C =:= $&; C =:= $'; C =:= $(;
                       C =:= $); C =:= $*; C =:= $+; C =:= $,; C =:= $;;
                       C =:= $=).
-define(unreserved(C), ?ALPHA(C); ?DIGIT(C); C =:= $-; C =:= $.; C =:= $_;
                       C =:= $~).
-define(reserved(C), ?gen_delims(C) orelse ?sub_delims(C)).


find(Bin) ->
    scheme(Bin, <<>>, []).


scheme(<<C, R/binary>>,  Acc, Out) when ?ALPHA(C) -> scheme1(R, <<Acc/binary, C>>, Out);
scheme(<<_, R/binary>>, _Acc, Out)                -> scheme(R, <<>>, Out);
scheme(<<>>,            _Acc, Out)                -> {ok, lists:reverse(Out)}.

scheme1(<<C,  R/binary>>, Acc, Out) when ?ALPHA(C) -> scheme1(R, <<Acc/binary, C>>, Out);
scheme1(<<C,  R/binary>>, Acc, Out) when ?DIGIT(C) -> scheme1(R, <<Acc/binary, C>>, Out);
scheme1(<<$+, R/binary>>, Acc, Out)                -> scheme1(R, <<Acc/binary, $+>>, Out);
scheme1(<<$-, R/binary>>, Acc, Out)                -> scheme1(R, <<Acc/binary, $->>, Out);
scheme1(<<$., R/binary>>, Acc, Out)                -> scheme1(R, <<Acc/binary, $.>>, Out);
scheme1(<<$:, R/binary>>, Acc, Out)                -> hier_part(R, <<Acc/binary, $:>>, Out);
scheme1(Else,            _Acc, Out)                -> scheme(Else, <<>>, Out).


hier_part(<<$/, $/, R/binary>>, Acc, Out) -> authority(R, 0, <<Acc/binary, $/, $/>>, Out);
hier_part(<<$/,     R/binary>>, Acc, Out) -> path_absolute(R, <<Acc/binary, $/>>, Out);
hier_part(<<C,          R/binary>>, Acc, Out) when ?unreserved(C); ?sub_delims(C); C == $:; C == $@ ->
    path_abempty(R, <<Acc/binary, C>>, Out);  % path-rootless
hier_part(<<$%, C1, C2, R/binary>>, Acc, Out) when ?HEXDIG(C1), ?HEXDIG(C2) ->
    path_abempty(R, <<Acc/binary, $%, C1, C2>>, Out);  % path-rootless
hier_part(<<$?, R/binary>>, Acc, Out) -> query(R, <<Acc/binary, $?>>, Out);
hier_part(<<$#, R/binary>>, Acc, Out) -> fragment(R, <<Acc/binary, $#>>, Out);
hier_part(Else,            _Acc, Out) -> scheme(Else, <<>>, Out).


%% Forward lookup to find which part to parse next.
authority(Bin, N, Acc, Out) ->
    case Bin of
        <<_:N/binary, $@, _/binary>> -> userinfo(Bin, Acc, Out);
        <<_:N/binary, $:, _/binary>> -> host(Bin, Acc, Out);
        <<_:N/binary, $/, _/binary>> -> host(Bin, Acc, Out);
        <<_:N/binary, $?, _/binary>> -> host(Bin, Acc, Out);
        <<_:N/binary, $#, _/binary>> -> host(Bin, Acc, Out);
        <<_:N/binary, _,  _/binary>> -> authority(Bin, N + 1, Acc, Out);
        <<_:N/binary>>               -> host(Bin, Acc, Out)
    end.

userinfo(<<C, R/binary>>, Acc, Out) when ?unreserved(C); ?sub_delims(C); C == $: ->
    userinfo(R, <<Acc/binary, C>>, Out);
userinfo(<<$%, C1, C2, R/binary>>, Acc, Out) when ?HEXDIG(C1), ?HEXDIG(C2) ->
    userinfo(R, <<Acc/binary, $%, C1, C2>>, Out);
userinfo(<<$@, R/binary>>, Acc, Out) ->
    host(R, <<Acc/binary, $@>>, Out);
userinfo(Else, _Acc, Out) ->
    scheme(Else, <<>>, Out).

host(<<$[, R/binary>>, Acc, Out) ->
    ip_literal(R, <<Acc/binary, $[>>, Out);
host(<<C, R/binary>>, Acc, Out) when ?unreserved(C); ?sub_delims(C) ->
    reg_name(R, <<Acc/binary, C>>, Out);
host(<<$%, C1, C2, R/binary>>, Acc, Out) when ?HEXDIG(C1), ?HEXDIG(C2) ->
    reg_name(R, <<Acc/binary, $%, C1, C2>>, Out);
host(Else, _Acc, Out) ->
    scheme(Else, <<>>, Out).


%% Too lazy to implement this. Consume until a closing ]
ip_literal(<<$], R/binary>>, Acc, Out) -> ip_literal1(R, <<Acc/binary, $]>>, Out);
ip_literal(<<C,  R/binary>>, Acc, Out) -> ip_literal(R, <<Acc/binary, C>>, Out);
ip_literal(<<>>,            _Acc, Out) -> {ok, lists:reverse(Out)}.

ip_literal1(<<$:, R/binary>>, Acc, Out) -> port(R, <<Acc/binary, $:>>, Out);
ip_literal1(<<$/, R/binary>>, Acc, Out) -> path_abempty(R, <<Acc/binary, $/>>, Out);
ip_literal1(<<$?, R/binary>>, Acc, Out) -> query(R, <<Acc/binary, $?>>, Out);
ip_literal1(<<$#, R/binary>>, Acc, Out) -> fragment(R, <<Acc/binary, $#>>, Out);
ip_literal1(Else,             Acc, Out) -> scheme(Else, <<>>, [Acc | Out]).


reg_name(<<C, R/binary>>, Acc, Out) when ?unreserved(C); ?sub_delims(C) ->
    reg_name(R, <<Acc/binary, C>>, Out);
reg_name(<<$%, C1, C2, R/binary>>, Acc, Out) when ?HEXDIG(C1), ?HEXDIG(C2) ->
    reg_name(R, <<Acc/binary, $%, C1, C2>>, Out);
reg_name(<<$:, R/binary>>, Acc, Out) -> port(R, <<Acc/binary, $:>>, Out);
reg_name(<<$/, R/binary>>, Acc, Out) -> path_abempty(R, <<Acc/binary, $/>>, Out);
reg_name(<<$?, R/binary>>, Acc, Out) -> query(R, <<Acc/binary, $?>>, Out);
reg_name(<<$#, R/binary>>, Acc, Out) -> fragment(R, <<Acc/binary, $#>>, Out);
reg_name(Else,             Acc, Out) -> scheme(Else, <<>>, [Acc | Out]).


port(<<C,  R/binary>>, Acc, Out) when ?DIGIT(C) -> port(R, <<Acc/binary, C>>, Out);
port(<<$/, R/binary>>, Acc, Out)                -> path_abempty(R, <<Acc/binary, $/>>, Out);
port(<<$?, R/binary>>, Acc, Out)                -> query(R, <<Acc/binary, $?>>, Out);
port(<<$#, R/binary>>, Acc, Out)                -> fragment(R, <<Acc/binary, $#>>, Out);
port(Else,             Acc, Out)                -> scheme(Else, <<>>, [Acc | Out]).


path_abempty(<<$/, R/binary>>, Acc, Out) -> path_abempty(R, <<Acc/binary, $/>>, Out);
path_abempty(<<C,  R/binary>>, Acc, Out) when ?unreserved(C); ?sub_delims(C); C == $:; C == $@ ->
    path_abempty(R, <<Acc/binary, C>>, Out);
path_abempty(<<$%, C1, C2, R/binary>>, Acc, Out) when ?HEXDIG(C1), ?HEXDIG(C2) ->
    path_abempty(R, <<Acc/binary, $%, C1, C2>>, Out);
path_abempty(<<$?, R/binary>>, Acc, Out) -> query(R, <<Acc/binary, $?>>, Out);
path_abempty(<<$#, R/binary>>, Acc, Out) -> fragment(R, <<Acc/binary, $#>>, Out);
path_abempty(Else,             Acc, Out) -> scheme(Else, <<>>, [Acc | Out]).


path_absolute(<<C, R/binary>>, Acc, Out) when ?unreserved(C); ?sub_delims(C); C == $:; C == $@ ->
    path_absolute(R, <<Acc/binary, C>>, Out);
path_absolute(<<$%, C1, C2, R/binary>>, Acc, Out) when ?HEXDIG(C1), ?HEXDIG(C2) ->
    path_absolute(R, <<Acc/binary, $%, C1, C2>>, Out);
path_absolute(<<$/, R/binary>>, Acc, Out) -> path_absolute1(R, <<Acc/binary, $/>>, Out);
path_absolute(<<$?, R/binary>>, Acc, Out) -> query(R, <<Acc/binary, $?>>, Out);
path_absolute(<<$#, R/binary>>, Acc, Out) -> fragment(R, <<Acc/binary, $#>>, Out);
path_absolute(Else,             Acc, Out) -> scheme(Else, <<>>, [Acc | Out]).

path_absolute1(<<$/, R/binary>>, Acc, Out) ->
    path_absolute1(R, <<Acc/binary, $/>>, Out);
path_absolute1(<<C, R/binary>>, Acc, Out) when ?unreserved(C); ?sub_delims(C); C == $:; C == $@ ->
    path_absolute1(R, <<Acc/binary, C>>, Out);
path_absolute1(<<$%, C1, C2, R/binary>>, Acc, Out) when ?HEXDIG(C1), ?HEXDIG(C2) ->
    path_absolute1(R, <<Acc/binary, $%, C1, C2>>, Out);
path_absolute1(<<$?, R/binary>>, Acc, Out) -> query(R, <<Acc/binary, $?>>, Out);
path_absolute1(<<$#, R/binary>>, Acc, Out) -> fragment(R, <<Acc/binary, $#>>, Out);
path_absolute1(Else,             Acc, Out) -> scheme(Else, <<>>, [Acc | Out]).


query(<<C, R/binary>>, Acc, Out) when ?unreserved(C); ?sub_delims(C); C == $:; C == $@; C == $/; C == $? ->
    query(R, <<Acc/binary, C>>, Out);
query(<<$%, C1, C2, R/binary>>, Acc, Out) when ?HEXDIG(C1), ?HEXDIG(C2) ->
    query(R, <<Acc/binary, $%, C1, C2>>, Out);
query(<<$#, R/binary>>, Acc, Out) -> fragment(R, <<Acc/binary, $#>>, Out);
query(Else,             Acc, Out) -> scheme(Else, <<>>, [Acc | Out]).


fragment(<<C, R/binary>>, Acc, Out) when ?unreserved(C); ?sub_delims(C); C == $:; C == $@; C == $/; C == $? ->
    fragment(R, <<Acc/binary, C>>, Out);
fragment(<<$%, C1, C2, R/binary>>, Acc, Out) when ?HEXDIG(C1), ?HEXDIG(C2) ->
    fragment(R, <<Acc/binary, $%, C1, C2>>, Out);
fragment(Else, Acc, Out) ->
    scheme(Else, <<>>, [Acc | Out]).
