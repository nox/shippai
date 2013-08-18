%% Copyright (c) 2013, Anthony Ramine <n.oxyde@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(shippai_transform_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([trivial/1]).

all() -> [trivial].

trivial(Config) when is_list(Config) ->
    Source = filename:join(?config(data_dir, Config), "shippai_trivial"),
    BeamDir = ?config(priv_dir, Config),
    Beam = filename:join(BeamDir, "shippai_trivial"),
    Opts = [return,{outdir,BeamDir}],
    {ok,shippai_trivial,[]} = compile:file(Source, Opts),
    {module,shippai_trivial} = code:load_abs(Beam),
    check(function_clause, fun shippai_trivial:function_clause/2, [43,foo]),
    check({case_clause,43}, fun shippai_trivial:case_clause/2, [43,foo]),
    check(if_clause, fun shippai_trivial:if_clause/2, [43,foo]),
    check({try_clause,43}, fun shippai_trivial:try_clause/2, [43,foo]),
    F = shippai_trivial:in_fun(foo),
    check({case_clause,bar}, F, [bar]).

check(Reason, F, As) ->
    {'EXIT',{Reason,[{_,_,As,_}|_]}} = (catch apply(F, As)),
    ok.
