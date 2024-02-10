%%%-------------------------------------------------------------------
%%% @author drastik <derezzed@protonmail.com>
%%% @copyright (C) 2024 hyperimpose.org
%%% SPDX-License-Identifier: BSD-3-Clause
%%%
%%% Created: 6 May 2023 by drastik <derezzed@protonmail.com>
%%%-------------------------------------------------------------------

-module(hi_iri).

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
-define(pct_encoded(C1, C2, C3), C1 =:= $%, ?HEXDIG(C2), ?HEXDIG(C3)).
-define(unreserved(C), ?ALPHA(C); ?DIGIT(C); C =:= $-; C =:= $.; C =:= $_;
                       C =:= $~).
-define(reserved(C), ?gen_delims(C) orelse ?sub_delims(C)).


-define(ucschar(C), (C >= 16#A0    andalso C =< 16#D7FF)
                  ; (C >= 16#F900  andalso C =< 16#FDCF)
                  ; (C >= 16#FDF0  andalso C =< 16#FFEF)
                  ; (C >= 16#10000 andalso C =< 16#1FFFD)
                  ; (C >= 16#20000 andalso C =< 16#2FFFD)
                  ; (C >= 16#30000 andalso C =< 16#3FFFD)
                  ; (C >= 16#40000 andalso C =< 16#4FFFD)
                  ; (C >= 16#50000 andalso C =< 16#5FFFD)
                  ; (C >= 16#60000 andalso C =< 16#6FFFD)
                  ; (C >= 16#70000 andalso C =< 16#7FFFD)
                  ; (C >= 16#80000 andalso C =< 16#8FFFD)
                  ; (C >= 16#90000 andalso C =< 16#9FFFD)
                  ; (C >= 16#A0000 andalso C =< 16#AFFFD)
                  ; (C >= 16#B0000 andalso C =< 16#BFFFD)
                  ; (C >= 16#C0000 andalso C =< 16#CFFFD)
                  ; (C >= 16#D0000 andalso C =< 16#DFFFD)
                  ; (C >= 16#E0000 andalso C =< 16#EFFFD)).

-define(iprivate(C), (C >= 16#E000   andalso C =< 16#F8FF)
                   ; (C >= 16#F0000  andalso C =< 16#FFFFD)
                   ; (C >= 16#100000 andalso C =< 16#10FFFD)).

-define(iunreserved(C), ?ALPHA(C); ?DIGIT(C); C =:= $-; C =:= $.; C =:= $_;
                       C =:= $~; ?ucschar(C)).

-define(ipchar1(C), ?iunreserved(C); ?sub_delims(C); C =:= $:; C =:= $@).
-define(ipchar2(C1, C2, C3), ?pct_encoded(C1, C2, C3)).


find(Input) ->
    case unicode:characters_to_binary(Input) of
        {_Error, Bin, _Rest} ->
            scheme(Bin, <<>>, []);
        Bin ->
            scheme(Bin, <<>>, [])
    end.


scheme(<<C/utf8, R/binary>>,  Acc, Out) when ?ALPHA(C) -> scheme1(R, <<Acc/binary, C/utf8>>, Out);
scheme(<<_/utf8, R/binary>>, _Acc, Out)                -> scheme(R, <<>>, Out);
scheme(<<>>,                 _Acc, Out)                -> {ok, lists:reverse(Out)}.

scheme1(<<C/utf8,  R/binary>>, Acc, Out) when ?ALPHA(C) -> scheme1(R, <<Acc/binary, C/utf8>>, Out);
scheme1(<<C/utf8,  R/binary>>, Acc, Out) when ?DIGIT(C) -> scheme1(R, <<Acc/binary, C/utf8>>, Out);
scheme1(<<$+, R/binary>>,      Acc, Out)                -> scheme1(R, <<Acc/binary, $+>>, Out);
scheme1(<<$-, R/binary>>,      Acc, Out)                -> scheme1(R, <<Acc/binary, $->>, Out);
scheme1(<<$., R/binary>>,      Acc, Out)                -> scheme1(R, <<Acc/binary, $.>>, Out);
scheme1(<<$:, R/binary>>,      Acc, Out)                -> ihier_part(R, <<Acc/binary, $:>>, Out);
scheme1(Else,                 _Acc, Out)                -> scheme(Else, <<>>, Out).


ihier_part(<<$/, $/, R/binary>>, Acc, Out) -> iauthority(R, 0, <<Acc/binary, $/, $/>>, Out);
ihier_part(<<$/,     R/binary>>, Acc, Out) -> ipath_absolute(R, <<Acc/binary, $/>>, Out);
ihier_part(<<C/utf8, R/binary>>, Acc, Out) when ?unreserved(C); ?sub_delims(C); C == $:; C == $@ ->
    ipath_abempty(R, <<Acc/binary, C/utf8>>, Out);  % path-rootless
ihier_part(<<$%, C1, C2, R/binary>>, Acc, Out) when ?pct_encoded($%, C1, C2) ->
    ipath_abempty(R, <<Acc/binary, $%, C1, C2>>, Out);  % path-rootless
ihier_part(<<$?, R/binary>>, Acc, Out) -> iquery(R, <<Acc/binary, $?>>, Out);
ihier_part(<<$#, R/binary>>, Acc, Out) -> ifragment(R, <<Acc/binary, $#>>, Out);
ihier_part(Else,            _Acc, Out) -> scheme(Else, <<>>, Out).


%% Forward lookup to find which part to parse next.
iauthority(Bin, N, Acc, Out) ->
    case Bin of
        <<_:N/binary, $@, _/binary>> -> iuserinfo(Bin, Acc, Out);
        <<_:N/binary, $:, _/binary>> -> ihost(Bin, Acc, Out);
        <<_:N/binary, $/, _/binary>> -> ihost(Bin, Acc, Out);
        <<_:N/binary, $?, _/binary>> -> ihost(Bin, Acc, Out);
        <<_:N/binary, $#, _/binary>> -> ihost(Bin, Acc, Out);
        <<_:N/binary, _,  _/binary>> -> iauthority(Bin, N + 1, Acc, Out);
        <<_:N/binary>>               -> ihost(Bin, Acc, Out)
    end.

iuserinfo(<<C/utf8, R/binary>>, Acc, Out) when ?iunreserved(C); ?sub_delims(C); C == $: ->
    iuserinfo(R, <<Acc/binary, C/utf8>>, Out);
iuserinfo(<<$%, C1, C2, R/binary>>, Acc, Out) when ?HEXDIG(C1), ?HEXDIG(C2) ->
    iuserinfo(R, <<Acc/binary, $%, C1, C2>>, Out);
iuserinfo(<<$@, R/binary>>, Acc, Out) ->
    ihost(R, <<Acc/binary, $@>>, Out);
iuserinfo(Else, _Acc, Out) ->
    scheme(Else, <<>>, Out).

ihost(<<$[, R/binary>>, Acc, Out) ->
    ip_literal(R, <<Acc/binary, $[>>, Out);
ihost(<<C/utf8, R/binary>>, Acc, Out) when ?iunreserved(C); ?sub_delims(C) ->
    ireg_name(R, <<Acc/binary, C/utf8>>, Out);
ihost(<<$%, C1, C2, R/binary>>, Acc, Out) when ?pct_encoded($%, C1, C2) ->
    ireg_name(R, <<Acc/binary, $%, C1, C2>>, Out);
ihost(Else, _Acc, Out) ->
    scheme(Else, <<>>, Out).


%% Too lazy to implement this. Consume until a closing ]
ip_literal(<<$], R/binary>>,      Acc, Out) -> ip_literal1(R, <<Acc/binary, $]>>, Out);
ip_literal(<<C/utf8,  R/binary>>, Acc, Out) -> ip_literal(R, <<Acc/binary, C/utf8>>, Out);
ip_literal(<<>>,                 _Acc, Out) -> {ok, lists:reverse(Out)}.

ip_literal1(<<$:, R/binary>>, Acc, Out) -> port(R, <<Acc/binary, $:>>, Out);
ip_literal1(<<$/, R/binary>>, Acc, Out) -> ipath_abempty(R, <<Acc/binary, $/>>, Out);
ip_literal1(<<$?, R/binary>>, Acc, Out) -> iquery(R, <<Acc/binary, $?>>, Out);
ip_literal1(<<$#, R/binary>>, Acc, Out) -> ifragment(R, <<Acc/binary, $#>>, Out);
ip_literal1(Else,             Acc, Out) -> scheme(Else, <<>>, [Acc | Out]).


ireg_name(<<C/utf8, R/binary>>, Acc, Out) when ?iunreserved(C); ?sub_delims(C) ->
    ireg_name(R, <<Acc/binary, C/utf8>>, Out);
ireg_name(<<$%, C1, C2, R/binary>>, Acc, Out) when ?pct_encoded($%, C1, C2) ->
    ireg_name(R, <<Acc/binary, $%, C1, C2>>, Out);
ireg_name(<<$:, R/binary>>, Acc, Out) -> port(R, <<Acc/binary, $:>>, Out);
ireg_name(<<$/, R/binary>>, Acc, Out) -> ipath_abempty(R, <<Acc/binary, $/>>, Out);
ireg_name(<<$?, R/binary>>, Acc, Out) -> iquery(R, <<Acc/binary, $?>>, Out);
ireg_name(<<$#, R/binary>>, Acc, Out) -> ifragment(R, <<Acc/binary, $#>>, Out);
ireg_name(Else,             Acc, Out) -> scheme(Else, <<>>, [Acc | Out]).


port(<<C/utf8,  R/binary>>, Acc, Out) when ?DIGIT(C) -> port(R, <<Acc/binary, C/utf8>>, Out);
port(<<$/, R/binary>>,      Acc, Out)                -> ipath_abempty(R, <<Acc/binary, $/>>, Out);
port(<<$?, R/binary>>,      Acc, Out)                -> iquery(R, <<Acc/binary, $?>>, Out);
port(<<$#, R/binary>>,      Acc, Out)                -> ifragment(R, <<Acc/binary, $#>>, Out);
port(Else,                  Acc, Out)                -> scheme(Else, <<>>, [Acc | Out]).


ipath_abempty(<<$/, R/binary>>, Acc, Out) -> ipath_abempty(R, <<Acc/binary, $/>>, Out);
ipath_abempty(<<C/utf8,  R/binary>>, Acc, Out) when ?ipchar1(C) ->
    ipath_abempty(R, <<Acc/binary, C/utf8>>, Out);
ipath_abempty(<<$%, C1, C2, R/binary>>, Acc, Out) when ?ipchar2($%, C1, C2) ->
    ipath_abempty(R, <<Acc/binary, $%, C1, C2>>, Out);
ipath_abempty(<<$?, R/binary>>, Acc, Out) -> iquery(R, <<Acc/binary, $?>>, Out);
ipath_abempty(<<$#, R/binary>>, Acc, Out) -> ifragment(R, <<Acc/binary, $#>>, Out);
ipath_abempty(Else,             Acc, Out) -> scheme(Else, <<>>, [Acc | Out]).


ipath_absolute(<<C/utf8, R/binary>>, Acc, Out) when ?ipchar1(C) ->
    ipath_absolute(R, <<Acc/binary, C/utf8>>, Out);
ipath_absolute(<<$%, C1, C2, R/binary>>, Acc, Out) when ?ipchar2($%, C1, C2) ->
    ipath_absolute(R, <<Acc/binary, $%, C1, C2>>, Out);
ipath_absolute(<<$/, R/binary>>, Acc, Out) -> ipath_absolute1(R, <<Acc/binary, $/>>, Out);
ipath_absolute(<<$?, R/binary>>, Acc, Out) -> iquery(R, <<Acc/binary, $?>>, Out);
ipath_absolute(<<$#, R/binary>>, Acc, Out) -> ifragment(R, <<Acc/binary, $#>>, Out);
ipath_absolute(Else,             Acc, Out) -> scheme(Else, <<>>, [Acc | Out]).

ipath_absolute1(<<$/, R/binary>>, Acc, Out) ->
    ipath_absolute1(R, <<Acc/binary, $/>>, Out);
ipath_absolute1(<<C/utf8, R/binary>>, Acc, Out) when ?ipchar1(C) ->
    ipath_absolute1(R, <<Acc/binary, C/utf8>>, Out);
ipath_absolute1(<<$%, C1, C2, R/binary>>, Acc, Out) when ?ipchar2($%, C1, C2) ->
    ipath_absolute1(R, <<Acc/binary, $%, C1, C2>>, Out);
ipath_absolute1(<<$?, R/binary>>, Acc, Out) -> iquery(R, <<Acc/binary, $?>>, Out);
ipath_absolute1(<<$#, R/binary>>, Acc, Out) -> ifragment(R, <<Acc/binary, $#>>, Out);
ipath_absolute1(Else,             Acc, Out) -> scheme(Else, <<>>, [Acc | Out]).


iquery(<<C/utf8, R/binary>>, Acc, Out) when ?ipchar1(C); ?iprivate(C); C =:= $/; C =:= $? ->
    iquery(R, <<Acc/binary, C/utf8>>, Out);
iquery(<<$%, C1, C2, R/binary>>, Acc, Out) when ?ipchar2($%, C1, C2) ->
    iquery(R, <<Acc/binary, $%, C1, C2>>, Out);
iquery(<<$#, R/binary>>, Acc, Out) -> ifragment(R, <<Acc/binary, $#>>, Out);
iquery(Else,             Acc, Out) -> scheme(Else, <<>>, [Acc | Out]).


ifragment(<<C/utf8, R/binary>>, Acc, Out) when ?ipchar1(C); C == $/; C == $? ->
    ifragment(R, <<Acc/binary, C/utf8>>, Out);
ifragment(<<$%, C1, C2, R/binary>>, Acc, Out) when ?ipchar2($%, C1, C2) ->
    ifragment(R, <<Acc/binary, $%, C1, C2>>, Out);
ifragment(Else, Acc, Out) ->
    scheme(Else, <<>>, [Acc | Out]).
