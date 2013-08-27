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

-spec core_transform(cerl:c_module(), _) -> cerl:c_module().
core_transform(Mod, _Opts) ->
    Defs = [ {Name,function(Fun)} || {Name,Fun} <- cerl:module_defs(Mod) ],
    cerl:update_c_module(Mod, cerl:module_name(Mod), cerl:module_exports(Mod),
                         cerl:module_attrs(Mod), Defs).

-spec function(cerl:c_fun()) -> cerl:c_fun().
function(Fun) ->
    case cerl:fun_arity(Fun) of
        0 -> Fun;
        Arity ->
            Args = cerl:fun_vars(Fun),
            Body = body(fresh_vars(Arity, Fun), Args, cerl:fun_body(Fun)),
            cerl:update_c_fun(Fun, Args, Body)
    end.

-spec body([cerl:c_var()], [cerl:c_var()], cerl:cerl()) -> cerl:cerl().
body(Vs, Args, Body) ->
    VsArg = cerl:make_list(Vs),
    cerl:c_let(Vs, cerl:c_values(Args), node(VsArg, Body)).

-spec node(cerl:c_cons(), cerl:cerl()) -> cerl:cerl().
node(VsArg, Node) ->
    case cerl:type(Node) of
        'fun' -> function(Node);
        primop -> primop(VsArg, Node);
        _ ->
            case cerl:is_leaf(Node) of
                true -> Node;
                false ->
                    cerl:update_tree(Node, subtrees(VsArg, cerl:subtrees(Node)))
            end
    end.

-spec primop(cerl:c_cons(), cerl:cerl()) -> cerl:cerl().
primop(VsArg, Op) ->
    Name = cerl:primop_name(Op),
    case cerl:is_c_atom(Name) andalso cerl:atom_val(Name) =:= match_fail of
        true -> match_fail_primop(VsArg, Op);
        false -> Op
    end.

-spec match_fail_primop(cerl:c_cons(), cerl:cerl()) -> cerl:cerl().
match_fail_primop(VsArg, Op) ->
    case cerl:primop_args(Op) of
        [Arg] ->
            case is_function_clause_arg(Arg) of
                true -> Op;
                false ->
                    cerl:update_c_call(Op, cerl:c_atom(erlang),
                                       cerl:c_atom(error), [Arg,VsArg])
            end;
        _ -> Op
    end.

-spec subtrees(cerl:c_cons(), [[cerl:cerl()]]) -> [[cerl:cerl()]].
subtrees(VsArg, Trees) ->
    [ [ node(VsArg, Node) || Node <- Group ] || Group <- Trees ].

-spec is_function_clause_arg(cerl:cerl()) -> boolean().
is_function_clause_arg(Arg) ->
    case cerl:is_c_tuple(Arg) of
        true ->
            case cerl:tuple_es(Arg) of
                [Tag|_] ->
                            cerl:is_c_atom(Tag)
                    andalso cerl:atom_val(Tag) =:= function_clause;
                _ -> false
            end;
        false -> false
    end.

-spec fresh_vars(non_neg_integer(), cerl:cerl()) -> [cerl:c_var()].
fresh_vars(N, Node) ->
    fresh_vars(N, cerl_trees:variables(Node), 0, []).

fresh_vars(0, _, _, Acc) ->
    Acc;
fresh_vars(N, Vs, I, Acc) ->
    Name = list_to_atom("_shippai" ++ integer_to_list(I)),
    case ordsets:is_element(Name, Vs) of
        true -> fresh_vars(N, Vs, I + 1, Acc);
        false -> fresh_vars(N - 1, Vs, I + 1, [cerl:c_var(Name)|Acc])
    end.
