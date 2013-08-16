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

-module(shippai_transform).

-export([core_transform/2]).

core_transform(Mod, _Opts) ->
    Defs = [ {Name,function(Fun)} || {Name,Fun} <- cerl:module_defs(Mod) ],
    cerl:update_c_module(Mod, cerl:module_name(Mod), cerl:module_exports(Mod),
                         cerl:module_attrs(Mod), Defs).

function(Fun) ->
    case cerl:fun_arity(Fun) of
        0 -> Fun;
        Arity ->
            Args = cerl:fun_vars(Fun),
            cerl:update_c_fun(Fun, Args,
                              body(fresh_vars(Arity), Args, cerl:fun_body(Fun)))
    end.

body(Vs, Args, Body) ->
    VsArg = cerl:make_list(Vs),
    cerl:c_let(Vs, cerl:c_values(Args),
               cerl_trees:map(fun (Node) -> node(VsArg, Node) end, Body)).

node(VsArg, Node) ->
    case cerl:type(Node) of
        'fun' -> function(Node);
        primop ->
            case cerl:atom_val(cerl:primop_name(Node)) of
                match_fail ->
                    [Arg] = cerl:primop_args(Node),
                    cerl:update_c_call(Node, cerl:c_atom(erlang),
                                       cerl:c_atom(error), [Arg,VsArg]);
                _ -> Node
            end;
        _ -> Node
    end.

fresh_vars(N) ->
    fresh_vars(N, []).

fresh_vars(0, Acc) ->
    Acc;
fresh_vars(N, Acc) ->
    Name = list_to_atom("_shippai" ++ integer_to_list(N)),
    fresh_vars(N - 1, [cerl:c_var(Name)|Acc]).
