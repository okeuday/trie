%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==A trie data structure implementation.==
%%% The trie (i.e., from "retrieval") data structure was invented by
%%% Edward Fredkin (it is a form of radix sort).  The implementation stores
%%% string suffixes as a list because it is a PATRICIA trie
%%% (PATRICIA - Practical Algorithm to Retrieve Information
%%%  Coded in Alphanumeric, D.R.Morrison (1968)).
%%%
%%% This Erlang trie implementation uses binary keys.  Using binary keys
%%% means that other data structures are quicker alternatives, so this
%%% module is probably not a good choice, unless it is used for functions
%%% not available elsewhere.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2010-2013, Michael Truog <mjtruog at gmail dot com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes software developed by Michael Truog
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2010-2013 Michael Truog
%%% @version 1.1.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(btrie).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([append/3,
         append_list/3,
         erase/2,
         fetch/2,
         fetch_keys/1,
         fetch_keys_similar/2,
         filter/2,
         find/2,
         fold/3,
         foldl/3,
         foldr/3,
         fold_similar/4,
         foldl_similar/4,
         foldr_similar/4,
         foreach/2,
         from_list/1,
         is_key/2,
         map/2,
         merge/3,
         new/0,
         new/1,
         prefix/3,
         size/1,
         store/2,
         store/3,
         to_list/1,
         update/3,
         update/4,
         update_counter/3,
         test/0]).

-define(MODE_BINARY, true).
-include("trie.hrl").
% temporarily don't use the header file
%-define(TYPE_EMPTY, <<>>).
%-define(TYPE_CHECK(V), is_binary(V)).
%-type trie_return() :: {integer(), integer(), tuple()}.
%-type trie() :: ?TYPE_EMPTY | trie_return().
%store(Key, Node) -> store(Key, empty, Node).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Erase a value in a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec erase(Key :: binary(),
            Node :: trie()) -> trie().

erase(_, ?TYPE_EMPTY = Node) ->
    Node;

erase(<<H:8,T/binary>>, Node) ->
    erase_node(H, T, Node).

erase_node(H, _, {I0, I1, _} = Node)
    when is_integer(H), H < I0;
         is_integer(H), H > I1 ->
    Node;

erase_node(H, T, {I0, I1, Data} = OldNode)
    when is_integer(H) ->
    I = H - I0 + 1,
    {Node, Value} = erlang:element(I, Data),
    if
        T == Node ->
            if
                Value =:= error ->
                    OldNode;
                true ->
                    {I0, I1, erlang:setelement(I, Data, {?TYPE_EMPTY, error})}
            end;
        T =:= ?TYPE_EMPTY ->
            if
                Value =:= error ->
                    OldNode;
                true ->
                    {I0, I1, erlang:setelement(I, Data, {Node, error})}
            end;
        ?TYPE_CHECK(Node) ->
            OldNode;
        is_tuple(Node) ->
            <<H1:8,T1/binary>> = T,
            {I0, I1, erlang:setelement(I, Data,
                {erase_node(H1, T1, Node), Value})}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Fetch a value from a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec fetch(binary(),
            trie_return()) -> any().

fetch(<<H:8,T/binary>>, {_, _, _} = Node) ->
    fetch_node(H, T, Node).

fetch_node(H, T, {I0, I1, Data})
    when is_integer(H), H >= I0, H =< I1 ->
    {Node, Value} = erlang:element(H - I0 + 1, Data),
    case T of
        ?TYPE_EMPTY ->
            if
                is_tuple(Node); Node =:= ?TYPE_EMPTY ->
                    if
                        Value =/= error ->
                            Value
                    end
            end;
        <<H1:8,T1/binary>> ->
            case Node of
                {_, _, _} ->
                    fetch_node(H1, T1, Node);
                T when Value =/= error ->
                    Value
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Filter a trie with a predicate function.===
%% @end
%%-------------------------------------------------------------------------

-spec filter(F :: fun((binary(), any()) -> boolean()),
             Node :: trie()) -> trie().

filter(F, ?TYPE_EMPTY = Node) when is_function(F, 2) ->
    Node;

filter(F, Node) when is_function(F, 2) ->
    filter_node(F, ?TYPE_EMPTY, Node).

filter_node(F, Key, {I0, I1, Data}) ->
    {I0, I1, filter_element(F, I1 - I0 + 1, I0 - 1, Key, Data)};

filter_node(_, _, Node)
    when ?TYPE_CHECK(Node) ->
    Node.

filter_element(_, 0, _, _, Data) ->
    Data;

filter_element(F, I, Offset, Key, Data) ->
    {Node, Value} = erlang:element(I, Data),
    Character = Offset + I,
    if
        Node =:= ?TYPE_EMPTY ->
            if
                Value =:= error ->
                    filter_element(F, I - 1, Offset, Key, Data);
                true ->
                    case F(<<Key/binary,Character:8>>, Value) of
                        true ->
                            filter_element(F, I - 1, Offset, Key, Data);
                        false ->
                            filter_element(F, I - 1, Offset, Key,
                                erlang:setelement(I, Data,
                                    {?TYPE_EMPTY, error}))
                    end
            end;
        Value =:= error ->
            filter_element(F, I - 1, Offset, Key, erlang:setelement(I, Data,
                {filter_node(F, <<Key/binary,Character:8>>, Node), Value}));
        true ->
            NewKey = <<Key/binary,Character:8>>,
            if
                ?TYPE_CHECK(Node) ->
                    case F(<<NewKey/binary,Node/binary>>, Value) of
                        true ->
                            filter_element(F, I - 1, Offset, Key, Data);
                        false ->
                            filter_element(F, I - 1, Offset, Key,
                                erlang:setelement(I, Data,
                                    {?TYPE_EMPTY, error}))
                    end;
                true ->
                    case F(NewKey, Value) of
                        true ->
                            filter_element(F, I - 1, Offset, Key, Data);
                        false ->
                            filter_element(F, I - 1, Offset, Key,
                                erlang:setelement(I, Data,
                                    {filter_node(F, NewKey, Node), error}))
                    end
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Find a value in a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec find(binary(), trie()) -> {ok, any()} | 'error'.

find(_, ?TYPE_EMPTY) ->
    error;

find(<<H:8,T/binary>>, {_, _, _} = Node) ->
    find_node(H, T, Node).

find_node(H, _, {I0, I1, _})
    when is_integer(H), H < I0;
         is_integer(H), H > I1 ->
    error;

find_node(H, ?TYPE_EMPTY, {I0, _, Data})
    when is_integer(H) ->
    {Node, Value} = erlang:element(H - I0 + 1, Data),
    if
        is_tuple(Node); Node =:= ?TYPE_EMPTY ->
            if
                Value =:= error ->
                    error;
                true ->
                    {ok, Value}
            end;
        true ->
            error
    end;

find_node(H, T, {I0, _, Data})
    when is_integer(H) ->
    {Node, Value} = erlang:element(H - I0 + 1, Data),
    case Node of
        {_, _, _} ->
            <<H1:8,T1/binary>> = T,
            find_node(H1, T1, Node);
        T ->
            if
                Value =:= error ->
                    error;
                true ->
                    {ok, Value}
            end;
        _ ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Fold a function over the trie.===
%% Traverses in alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec foldl(F :: fun((binary(), any(), any()) -> any()),
            A :: any(),
            Node :: trie()) -> any().

foldl(F, A, ?TYPE_EMPTY) when is_function(F, 3) ->
    A;

foldl(F, A, Node) when is_function(F, 3) ->
    foldl(F, A, ?TYPE_EMPTY, Node).

foldl(F, A, Key, {I0, I1, Data}) ->
    foldl_element(F, A, 1, I1 - I0 + 2, I0 - 1, Key, Data).

foldl_element(_, A, N, N, _, _, _) ->
    A;

foldl_element(F, A, I, N, Offset, Key, Data) ->
    {Node, Value} = erlang:element(I, Data),
    Character = Offset + I,
    if
        ?TYPE_CHECK(Node) =:= false ->
            if
                Value =:= error ->
                    foldl_element(F,
                        foldl(F, A, <<Key/binary,Character:8>>, Node),
                        I + 1, N, Offset, Key, Data);
                true ->
                    NewKey = <<Key/binary,Character:8>>,
                    foldl_element(F,
                        foldl(F, F(NewKey, Value, A), NewKey, Node),
                        I + 1, N, Offset, Key, Data)
            end;
        true ->
            if
                Value =:= error ->
                    foldl_element(F, A,
                        I + 1, N, Offset, Key, Data);
                true ->
                    foldl_element(F,
                        F(<<Key/binary,Character:8,Node/binary>>, Value, A),
                        I + 1, N, Offset, Key, Data)
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Fold a function over the trie in reverse.===
%% Traverses in reverse alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec foldr(F :: fun((binary(), any(), any()) -> any()),
            A :: any(),
            Node :: trie()) -> any().

foldr(F, A, ?TYPE_EMPTY) when is_function(F, 3) ->
    A;

foldr(F, A, Node) when is_function(F, 3) ->
    foldr(F, A, ?TYPE_EMPTY, Node).

foldr(F, A, Key, {I0, I1, Data}) ->
    foldr_element(F, A, I1 - I0 + 1, I0 - 1, Key, Data).

foldr_element(_, A, 0, _, _, _) ->
    A;

foldr_element(F, A, I, Offset, Key, Data) ->
    {Node, Value} = erlang:element(I, Data),
    Character = Offset + I,
    if
        ?TYPE_CHECK(Node) =:= false ->
            if
                Value =:= error ->
                    foldr_element(F,
                        foldr(F, A, <<Key/binary,Character:8>>, Node),
                        I - 1, Offset, Key, Data);
                true ->
                    NewKey = <<Key/binary,Character:8>>,
                    foldr_element(F,
                        F(NewKey, Value, foldr(F, A, NewKey, Node)),
                        I - 1, Offset, Key, Data)
            end;
        true ->
            if
                Value =:= error ->
                    foldr_element(F, A,
                        I - 1, Offset, Key, Data);
                true ->
                    foldr_element(F,
                        F(<<Key/binary,Character:8,Node/binary>>, Value, A),
                        I - 1, Offset, Key, Data)
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Fold a function over the keys within a trie that share a common prefix.===
%% Traverses in alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec foldl_similar(Similar :: binary(),
                    F :: fun((binary(), any(), any()) -> any()),
                    A :: any(),
                    Node :: trie()) -> any().

foldl_similar(<<H:8,_/binary>>, _, A, {I0, I1, _})
    when is_integer(H), H < I0;
         is_integer(H), H > I1 ->
    A;

foldl_similar(_, _, A, ?TYPE_EMPTY) ->
    A;

foldl_similar(<<H:8,T/binary>>, F, A, Node) ->
    fold_similar_node(H, T, foldl, F, A, ?TYPE_EMPTY, error, Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Fold a function over the keys within a trie that share a common prefix in reverse.===
%% Traverses in reverse alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec foldr_similar(Similar :: binary(),
                    F :: fun((binary(), any(), any()) -> any()),
                    A :: any(),
                    Node :: trie()) -> any().

foldr_similar(<<H:8,_/binary>>, _, A, {I0, I1, _})
    when is_integer(H), H < I0;
         is_integer(H), H > I1 ->
    A;

foldr_similar(_, _, A, ?TYPE_EMPTY) ->
    A;

foldr_similar(<<H:8,T/binary>>, F, A, Node) ->
    fold_similar_node(H, T, foldr, F, A, ?TYPE_EMPTY, error, Node).

fold_similar_node(H, _, Fold, F, A, Key, LastValue, {I0, I1, _} = Node)
    when is_integer(H), H < I0;
         is_integer(H), H > I1 ->
    if
        LastValue =:= error ->
            fold_similar_element(Fold, F, A, Key, Node);
        Fold =:= foldl ->
            fold_similar_element(Fold, F, F(Key, LastValue, A), Key, Node);
        Fold =:= foldr ->
            F(Key, LastValue, fold_similar_element(Fold, F, A, Key, Node))
    end;

fold_similar_node(H, ?TYPE_EMPTY, Fold, F, A, Key, _, {I0, _, Data} = Node)
    when is_integer(H) ->
    {ChildNode, Value} = erlang:element(H - I0 + 1, Data),
    if
        is_tuple(ChildNode) ->
            NewKey = <<Key/binary,H:8>>,
            if
                Value =:= error ->
                    fold_similar_element(Fold, F, A, NewKey, ChildNode);
                Fold =:= foldl ->
                    fold_similar_element(Fold, F, F(NewKey, Value, A),
                                         NewKey, ChildNode);
                Fold =:= foldr ->
                    F(NewKey, Value,
                      fold_similar_element(Fold, F, A, NewKey, ChildNode))
            end;
        Value =/= error, ChildNode =:= ?TYPE_EMPTY ->
            F(<<Key/binary,H:8>>, Value, A);
        true ->
            fold_similar_element(Fold, F, A, Key, Node)
    end;

fold_similar_node(H, T, Fold, F, A, Key, _, {I0, _, Data} = Node)
    when is_integer(H) ->
    {ChildNode, Value} = erlang:element(H - I0 + 1, Data),
    if
        is_tuple(ChildNode) ->
            <<H1:8,T1/binary>> = T,
            fold_similar_node(H1, T1, Fold, F, A,
                <<Key/binary,H:8>>, Value, ChildNode);
        Value =/= error, ChildNode == T ->
            F(<<Key/binary,H:8,T/binary>>, Value, A);
        true ->
            fold_similar_element(Fold, F, A, Key, Node)
    end.

fold_similar_element(foldl, F, A, Key, Node) ->
    foldl(F, A, Key, Node);

fold_similar_element(foldr, F, A, Key, Node) ->
    foldr(F, A, Key, Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Call a function for each element.===
%% Traverses in alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec foreach(F :: fun((binary(), any()) -> any()),
              Node :: trie()) -> any().

foreach(F, ?TYPE_EMPTY) when is_function(F, 2) ->
    ok;

foreach(F, Node) when is_function(F, 2) ->
    foreach(F, ?TYPE_EMPTY, Node).

foreach(F, Key, {I0, I1, Data}) ->
    foreach_element(F, 1, I1 - I0 + 2, I0 - 1, Key, Data).

foreach_element(_, N, N, _, _, _) ->
    ok;

foreach_element(F, I, N, Offset, Key, Data) ->
    {Node, Value} = erlang:element(I, Data),
    Character = Offset + I,
    if
        ?TYPE_CHECK(Node) =:= false ->
            if
                Value =:= error ->
                    foreach(F, <<Key/binary,Character:8>>, Node),
                    foreach_element(F, I + 1, N, Offset, Key, Data);
                true ->
                    NewKey = <<Key/binary,Character:8>>,
                    F(NewKey, Value),
                    foreach(F, NewKey, Node),
                    foreach_element(F, I + 1, N, Offset, Key, Data)
            end;
        true ->
            if
                Value =:= error ->
                    foreach_element(F, I + 1, N, Offset, Key, Data);
                true ->
                    F(<<Key/binary,Character:8,Node/binary>>, Value),
                    foreach_element(F, I + 1, N, Offset, Key, Data)
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Determine if a key exists in a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec is_key(binary(), trie()) -> boolean().

is_key(_, ?TYPE_EMPTY) ->
    false;

is_key(<<H:8,T/binary>>, {_, _, _} = Node) ->
    is_key_node(H, T, Node).

is_key_node(H, _, {I0, I1, _})
    when is_integer(H), H < I0;
         is_integer(H), H > I1 ->
    false;

is_key_node(H, ?TYPE_EMPTY, {I0, _, Data})
    when is_integer(H) ->
    {Node, Value} = erlang:element(H - I0 + 1, Data),
    if
        is_tuple(Node); Node =:= ?TYPE_EMPTY ->
            (Value =/= error);
        true ->
            false
    end;

is_key_node(H, T, {I0, _, Data})
    when is_integer(H) ->
    {Node, Value} = erlang:element(H - I0 + 1, Data),
    case Node of
        {_, _, _} ->
            <<H1:8,T1/binary>> = T,
            is_key_node(H1, T1, Node);
        T ->
            (Value =/= error);
        _ ->
            false
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Map a function over a trie.===
%% Traverses in reverse alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec map(F :: fun((binary(), any()) -> any()),
          Node :: trie()) -> trie().

map(F, ?TYPE_EMPTY = Node) when is_function(F, 2) ->
    Node;

map(F, Node) when is_function(F, 2) ->
    map_node(F, ?TYPE_EMPTY, Node).

map_node(F, Key, {I0, I1, Data}) ->
    {I0, I1, map_element(F, I1 - I0 + 1, I0 - 1, Key, Data)};

map_node(_, _, Node)
    when ?TYPE_CHECK(Node) ->
    Node.

map_element(_, 0, _, _, Data) ->
    Data;

map_element(F, I, Offset, Key, Data) ->
    {Node, Value} = erlang:element(I, Data),
    Character = Offset + I,
    NewKey = <<Key/binary,Character:8>>,
    if
        Node =:= ?TYPE_EMPTY ->
            if
                Value =:= error ->
                    map_element(F, I - 1, Offset, Key, Data);
                true ->
                    map_element(F, I - 1, Offset, Key,
                        erlang:setelement(I, Data, {Node, F(NewKey, Value)}))
            end;
        Value =:= error ->
            map_element(F, I - 1, Offset, Key, erlang:setelement(I, Data,
                {map_node(F, NewKey, Node), Value}));
        ?TYPE_CHECK(Node) ->
            map_element(F, I - 1, Offset, Key, erlang:setelement(I, Data,
                {map_node(F, NewKey, Node),
                 F(<<NewKey/binary,Node/binary>>, Value)}));
        true ->
            map_element(F, I - 1, Offset, Key, erlang:setelement(I, Data,
                {map_node(F, NewKey, Node), F(NewKey, Value)}))
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a key/value pair in a trie instance.===
%% @end
%%-------------------------------------------------------------------------

-spec store(Key :: binary(),
            NewValue :: any(),
            Node :: trie()) -> trie_return().

store(<<H:8,T/binary>>, NewValue, ?TYPE_EMPTY) ->
    {H, H, {{T, NewValue}}};

store(<<H:8,T/binary>>, NewValue, Node) ->
    store_node(H, T, NewValue, Node).

store_node(H, T, NewValue, {I0, I1, Data})
    when is_integer(H), H < I0 ->
    NewData = erlang:setelement(1,
        tuple_move(I0 - H + 1, I1 - H + 1, Data, {?TYPE_EMPTY, error}),
        {T, NewValue}),
    {H, I1, NewData};

store_node(H, T, NewValue, {I0, I1, Data})
    when is_integer(H), H > I1 ->
    N = H - I0 + 1,
    NewData = erlang:setelement(N,
        tuple_move(1, N, Data, {?TYPE_EMPTY, error}),
        {T, NewValue}),
    {I0, H, NewData};

store_node(H, ?TYPE_EMPTY = T, NewValue, {I0, I1, Data})
    when is_integer(H) ->
    I = H - I0 + 1,
    {Node, Value} = erlang:element(I, Data),
    if
        is_tuple(Node); Node =:= ?TYPE_EMPTY ->
            {I0, I1, erlang:setelement(I, Data, {Node, NewValue})};
        true ->
            NewNode = {I0, I1, erlang:setelement(I, Data,
                new_instance_state(Node, Value, error))},
            store_node(H, T, NewValue, NewNode)
    end;

store_node(H, <<H1:8,T1/binary>> = T, NewValue, {I0, I1, Data})
    when is_integer(H) ->
    I = H - I0 + 1,
    {Node, Value} = erlang:element(I, Data),
    case Node of
        {_, _, _} ->
            {I0, I1, erlang:setelement(I, Data,
                {store_node(H1, T1, NewValue, Node), Value})};
        T ->
            {I0, I1, erlang:setelement(I, Data, {Node, Value})};
        ?TYPE_EMPTY ->
            if
                Value =:= error ->
                    {I0, I1, erlang:setelement(I, Data, {T, NewValue})};
                true ->
                    {I0, I1, erlang:setelement(I, Data,
                        new_instance_state(T, NewValue, Value))}
            end;
        <<BH:8,BT/binary>> ->
            NewNode = {I0, I1,
                erlang:setelement(I, Data, {{BH, BH, {{BT, Value}}}, error})},
            store_node(H, T, NewValue, NewNode)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Update a value in a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec update(binary(),
             F :: fun((any()) -> any()),
             trie_return()) -> trie_return().

update(<<H:8,T/binary>>, F, {_, _, _} = Node)
    when is_function(F, 1) ->
    update_node(H, T, F, Node).

update_node(H, ?TYPE_EMPTY, F, {I0, I1, Data})
    when is_integer(H), H >= I0, H =< I1 ->
    I = H - I0 + 1,
    {Node, Value} = erlang:element(I, Data),
    if
        is_tuple(Node); Node =:= ?TYPE_EMPTY, Value =/= error ->
            {I0, I1, erlang:setelement(I, Data, {Node, F(Value)})}
    end;

update_node(H, T, F, {I0, I1, Data})
    when is_integer(H), H >= I0, H =< I1 ->
    I = H - I0 + 1,
    {Node, Value} = erlang:element(I, Data),
    case Node of
        {_, _, _} ->
            <<H1:8,T1/binary>> = T,
            {I0, I1, erlang:setelement(I, Data,
                {update_node(H1, T1, F, Node), Value})};
        T ->
            true = Value =/= error,
            {I0, I1, erlang:setelement(I, Data, {Node, F(Value)})}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Update or add a value in a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec update(Key :: binary(),
             F :: fun((any()) -> any()),
             Initial :: any(),
             Node :: trie()) -> trie_return().

update(Key, _, Initial, ?TYPE_EMPTY = Node) ->
    store(Key, Initial, Node);

update(<<H:8,T/binary>>, F, Initial, {_, _, _} = Node)
    when is_function(F, 1) ->
    update_node(H, T, F, Initial, Node).

update_node(H, T, _, Initial, {I0, I1, Data})
    when is_integer(H), H < I0 ->
    NewData = erlang:setelement(1,
        tuple_move(I0 - H + 1, I1 - H + 1, Data, {?TYPE_EMPTY, error}),
        {T, Initial}),
    {H, I1, NewData};

update_node(H, T, _, Initial, {I0, I1, Data})
    when is_integer(H), H > I1 ->
    N = H - I0 + 1,
    NewData = erlang:setelement(N,
        tuple_move(1, N, Data, {?TYPE_EMPTY, error}),
        {T, Initial}),
    {I0, H, NewData};

update_node(H, ?TYPE_EMPTY = T, F, Initial, {I0, I1, Data})
    when is_integer(H) ->
    I = H - I0 + 1,
    {Node, Value} = erlang:element(I, Data),
    if
        is_tuple(Node); Node =:= ?TYPE_EMPTY ->
            if
                Value =:= error ->
                    {I0, I1, erlang:setelement(I, Data, {Node, Initial})};
                true ->
                    {I0, I1, erlang:setelement(I, Data, {Node, F(Value)})}
            end;
        true ->
            <<BH:8,BT/binary>> = Node,
            NewNode = {I0, I1,
               erlang:setelement(I, Data, {{BH, BH, {{BT, Value}}}, error})},
            update_node(H, T, F, Initial, NewNode)
    end;

update_node(H, T, F, Initial, {I0, I1, Data})
    when is_integer(H) ->
    I = H - I0 + 1,
    {Node, Value} = erlang:element(I, Data),
    case Node of
        {_, _, _} ->
            <<H1:8,T1/binary>> = T,
            {I0, I1, erlang:setelement(I, Data,
                {update_node(H1, T1, F, Initial, Node), Value})};
        T ->
            {I0, I1, erlang:setelement(I, Data, {Node, F(Value)})};
        ?TYPE_EMPTY ->
            if
                Value =:= error ->
                    {I0, I1, erlang:setelement(I, Data, {T, Initial})};
                true ->
                    {I0, I1, erlang:setelement(I, Data,
                        new_instance_state(T, Initial, Value))}
            end;
        <<BH:8,BT/binary>> ->
            NewNode = {I0, I1,
                erlang:setelement(I, Data, {{BH, BH, {{BT, Value}}}, error})},
            update_node(H, T, F, Initial, NewNode)
    end.

%%-------------------------------------------------------------------------
%% @private
%% @doc
%% ===Regression test.===
%% @end
%%-------------------------------------------------------------------------

test() ->
    {97,97,{{<<>>,empty}}} = btrie:new([<<"a">>]),
    {97,97,{{<<"b">>,empty}}} = btrie:new([<<"ab">>]),
    {97,97,{{<<"bc">>,empty}}} = btrie:new([<<"abc">>]),
    {97,97,{{<<"b">>,empty}}} = btrie:new([<<"ab">>]),
    {97,97,{{{97,98,{{<<>>,empty},{<<>>,empty}}},error}}} = 
        btrie:new([<<"ab">>,<<"aa">>]),
    {97,97,{{{97,98,{{<<"c">>,empty},{<<"c">>,empty}}},error}}} =
        btrie:new([<<"abc">>,<<"aac">>]),
    {97,97,{{{97,98,{{<<"c">>,2},{<<"c">>,1}}},error}}} =
        btrie:new([{<<"abc">>, 1},{<<"aac">>, 2}]),
    {97,97,{{{97,98,{{<<"c">>,2},{<<"cdefghijklmnopqrstuvwxyz">>,1}}},error}}} =
        RootNode0 = btrie:new([
            {<<"abcdefghijklmnopqrstuvwxyz">>, 1},{<<"aac">>, 2}]),
    {ok, 1} = btrie:find(<<"abcdefghijklmnopqrstuvwxyz">>, RootNode0),
    error = btrie:find(<<"abcdefghijklmnopqrstuvwxy">>, RootNode0),
%    {ok, 1} = btrie:find_prefix("abcdefghijklmnopqrstuvwxyz", RootNode0),
%    prefix = btrie:find_prefix("abcdefghijklmnopqrstuvwxy", RootNode0),
%    error = btrie:find_prefix("abcdefghijklmnopqrstuvwxyzX", RootNode0),
%    prefix = btrie:find_prefix("a", RootNode0),
%    prefix = btrie:find_prefix("aa", RootNode0),
%    {ok, 2} = btrie:find_prefix("aac", RootNode0),
%    error = btrie:find_prefix("aacX", RootNode0),
    {97,97,{{{97,98,{{{98,99,{{<<"cde">>,3},{<<>>,2}}},error},
     {<<"cdefghijklmnopqrstuvwxyz">>,1}}},error}}} =
        RootNode1 = btrie:store(<<"aabcde">>, 3, RootNode0),
    {97,97,{{{97,98,{{{98,99,{{<<"cde">>,13},{<<>>,12}}},error},
     {<<"cdefghijklmnopqrstuvwxyz">>,11}}},error}}} =
        map(fun(_, V) -> V + 10 end, RootNode1),
    {97,97,{{{97,98,{{{98,99,{{<<>>,error},{<<>>,error}}},error},
     {<<"cdefghijklmnopqrstuvwxyz">>,1}}},error}}} =
        filter(fun(_, V) -> V =< 1 end, RootNode1),
    {97,97,{{{97,98,{{{98,99,{{<<>>,error},{<<>>,2}}},error},
     {<<"cdefghijklmnopqrstuvwxyz">>,1}}},error}}} =
        filter(fun(_, V) -> V =< 2 end, RootNode1),
    [<<"aabcde">>, <<"aac">>, <<"abcdefghijklmnopqrstuvwxyz">>] =
        btrie:fetch_keys(RootNode1),
    [{<<"aabcde">>, 3}, {<<"aac">>, 2},
     {<<"abcdefghijklmnopqrstuvwxyz">>, 1}] = btrie:to_list(RootNode1),
    [{<<"aabcde">>, 3}, {<<"aac">>, 12},
     {<<"abcdefghijklmnopqrstuvwxyz">>, 1}] =
        btrie:to_list(btrie:update(
            <<"aac">>, fun(I) -> I + 10 end, RootNode1)),
    [{<<"aaa">>, 4}, {<<"aabcde">>, 3}, {<<"aac">>, 2},
     {<<"abcdefghijklmnopqrstuvwxyz">>, 1}] =
        btrie:to_list(btrie:update(
            <<"aaa">>, fun(I) -> I + 10 end, 4, RootNode1)),
    6 = foldl(fun(_, I, A) -> I + A end, 0, RootNode1),
    [{<<"aabcde">>, 3},{<<"aac">>, 2},{<<"abcdefghijklmnopqrstuvwxyz">>, 1}] =
        foldr(fun(K, V, A) -> [{K,V} | A] end, [], RootNode1),
    [{<<"abcdefghijklmnopqrstuvwxyz">>, 1}, {<<"aac">>, 2}, {<<"aabcde">>, 3}] =
        foldl(fun(K, V, A) -> [{K,V} | A] end, [], RootNode1),
    error = btrie:find(<<"aabcde">>, RootNode0),
    {ok, 3} = btrie:find(<<"aabcde">>, RootNode1),
    RootNode2 = btrie:erase(<<"aac">>, RootNode0),
    {ok, 1} = btrie:find(<<"abcdefghijklmnopqrstuvwxyz">>, RootNode2),
    {97,98,{{{98,98,{{<<>>,[2]}}},[1]},{<<"c">>,[3]}}} =
        RootNode3 = btrie:new([{<<"a">>, [1]},{<<"ab">>, [2]},{<<"bc">>, [3]}]),
    {97,98,{{{98,98,{{<<>>,[2]}}},[1,2]},{<<"c">>,[3]}}} =
        btrie:append(<<"a">>, 2, RootNode3),

    RootNode4 = btrie:new([
        {<<"ammmmmmm">>,      7},
        {<<"aaaaaaaaaaa">>,   4},
        {<<"aaa">>,           2},
        {<<"ab">>,            0},
        {<<"ab">>,            5},
        {<<"aa">>,            1},
        {<<"aba">>,           6},
        {<<"aaaaaaaa">>,      3}]),
    {97,97,
     {{{97,109,
        {{{97,97,
           {{{97,97,
              {{{97,97,
                 {{{97,97,
                    {{{97,97,
                       {{{97,97,
                          {{{97,97,
                             {{<<"aa">>,4}}},
                            3}}},
                         error}}},
                     error}}},
                   error}}},
                error}}},
             2}}},
          1},
         {{97,97,{{<<>>,6}}},5},
         {<<>>,error},
         {<<>>,error},
         {<<>>,error},
         {<<>>,error},
         {<<>>,error},
         {<<>>,error},
         {<<>>,error},
         {<<>>,error},
         {<<>>,error},
         {<<>>,error},
         {<<"mmmmmm">>,7}}},
       error}}} = RootNode4,
    [{<<"aa">>,1},
     {<<"aaa">>,2},
     {<<"aaaaaaaa">>,3},
     {<<"aaaaaaaaaaa">>,4},
     {<<"ab">>,5},
     {<<"aba">>,6},
     {<<"ammmmmmm">>,7}] = btrie:to_list(
        btrie:from_list(btrie:to_list(RootNode4))),
%    Liter =  ["aa", "aaa", "aaaaaaaa", "aaaaaaaaaaa", "ab", "aba"],
%    Fiter = fun(Key, _, Iter) ->
%        case lists:member(Key, Liter) of
%            true ->
%                Iter();
%            false ->
%                done
%        end
%    end,
%    Fitera = fun
%        (_, _, [], _) ->
%            done;
%        (Key, _, [Key | T], Iter) ->
%            Iter(T)
%    end,
%    ok = btrie:iter(Fiter, RootNode4),
%    done = btrie:itera(Fitera, Liter, RootNode4),
%    % btrie:map happens to go through in reverse order
    [<<"aa">>,
     <<"aaa">>,
     <<"aaaaaaaa">>,
     <<"aaaaaaaaaaa">>,
     <<"ab">>,
     <<"aba">>,
     <<"ammmmmmm">>] = btrie:fetch_keys(RootNode4),
    [<<"aa">>,
     <<"aaa">>,
     <<"aaaaaaaa">>,
     <<"aaaaaaaaaaa">>,
     <<"ab">>,
     <<"aba">>,
     <<"ammmmmmm">>] = btrie:foldr(
        fun(Key, _, L) -> [Key | L] end, [], RootNode4),
    [<<"ammmmmmm">>,
     <<"aba">>,
     <<"ab">>,
     <<"aaaaaaaaaaa">>,
     <<"aaaaaaaa">>,
     <<"aaa">>,
     <<"aa">>] = btrie:foldl(
        fun(Key, _, L) -> [Key | L] end, [], RootNode4),
    RootNode5 = btrie:store(<<"a">>, 0,
        btrie:store(<<"aaaa">>, 2.5, RootNode4)),
    {ok, 2.5} = btrie:find(<<"aaaa">>, RootNode5),
    error = btrie:find(<<"aaaa">>, RootNode4),
%    {ok, 2.5} = btrie:find_prefix("aaaa", RootNode5),
%    prefix = btrie:find_prefix("aaaa", RootNode4),
    2.5 = btrie:fetch(<<"aaaa">>, RootNode5),
    {'EXIT', {if_clause, _}} = (catch btrie:fetch(<<"aaaa">>, RootNode4)),
    RootNode4 = btrie:erase(<<"a">>, btrie:erase(<<"aaaa">>, RootNode5)),
    true = btrie:is_key(<<"aaaa">>, RootNode5),
    false = btrie:is_key(<<"aaaa">>, RootNode4),
    [<<"aa">>,
     <<"aaa">>,
     <<"aaaaaaaa">>,
     <<"aaaaaaaaaaa">>] = btrie:fetch_keys_similar(<<"aa">>, RootNode4),
    [<<"aaa">>,
     <<"aaaaaaaa">>,
     <<"aaaaaaaaaaa">>] = btrie:fetch_keys_similar(<<"aaac">>, RootNode4),
    [<<"ab">>,
     <<"aba">>] = btrie:fetch_keys_similar(<<"abba">>, RootNode4),
    [<<"aa">>,
     <<"aaa">>,
     <<"aaaaaaaa">>,
     <<"aaaaaaaaaaa">>,
     <<"ab">>,
     <<"aba">>,
     <<"ammmmmmm">>] = btrie:fetch_keys_similar(<<"a">>, RootNode4),
    [] = btrie:fetch_keys_similar(<<"b">>, RootNode4),
%    {ok, "aa", 1} = btrie:find_similar("aa", RootNode4),
%    {ok, "aaa", 2} = btrie:find_similar("aaac", RootNode4),
%    {ok, "aaaaaaaa", 3} = btrie:find_similar("aaaa", RootNode4),
%    {ok, "ab", 5} = btrie:find_similar("abba", RootNode4),
%    {ok, "aa", 1} = btrie:find_similar("a", RootNode4),
%    true = btrie:is_prefixed("abacus", RootNode4),
%    false = btrie:is_prefixed("ac", RootNode4),
%    false = btrie:is_prefixed("abacus", "ab", RootNode4),
%    true = btrie:foldl(fun(K, _, L) -> [K | L] end, [], RootNode4) ==
%           btrie:fold_match("*", fun(K, _, L) -> [K | L] end, [], RootNode4),
%    ["aaa"
%     ] = btrie:fold_match("*aa", fun(K, _, L) -> [K | L] end, [], RootNode4),
%    ["aaaaaaaaaaa",
%     "aaaaaaaa",
%     "aaa"
%     ] = btrie:fold_match("aa*", fun(K, _, L) -> [K | L] end, [], RootNode4),
%    ["aba"
%     ] = btrie:fold_match("ab*", fun(K, _, L) -> [K | L] end, [], RootNode4),
%    ["ammmmmmm"
%     ] = btrie:fold_match("am*", fun(K, _, L) -> [K | L] end, [], RootNode4),
%    ["aba",
%     "aaa"
%     ] = btrie:fold_match("a*a", fun(K, _, L) -> [K | L] end, [], RootNode4),
%    {'EXIT',badarg} = (catch btrie:fold_match("a**a", fun(K, _, L) -> [K | L] end, [], RootNode4)),
    _RootNode6 = btrie:new([
        {<<"*">>,      1},
        {<<"aa*">>,    2},
        {<<"aa*b">>,   3},
        {<<"aa*a*">>,  4},
        {<<"aaaaa">>,  5}]),
%    {ok,"aa*",2} = btrie:find_match("aaaa", RootNode6),
%    {ok,"aaaaa",5} = btrie:find_match("aaaaa", RootNode6),
%    {ok,"*",1} = btrie:find_match("aa", RootNode6),
%    {ok,"aa*",2} = btrie:find_match("aab", RootNode6),
%    {ok,"aa*b",3} = btrie:find_match("aabb", RootNode6),
%    {ok,"aa*a*",4} = btrie:find_match("aabab", RootNode6),
%    {ok,"aa*a*",4} = btrie:find_match("aababb", RootNode6),
%    {ok,"aa*a*",4} = btrie:find_match("aabbab", RootNode6),
%    {ok,"aa*a*",4} = btrie:find_match("aabbabb", RootNode6),
%    {'EXIT',badarg} = (catch btrie:find_match("aa*", RootNode6)),
%    {'EXIT',badarg} = (catch btrie:find_match("aaaa*", RootNode6)),
%    {'EXIT',badarg} = (catch btrie:find_match("aaaaa*", RootNode6)),
%    ["aa"] = btrie:pattern_parse("aa*", "aaaa"),
%    ["b"] = btrie:pattern_parse("aa*", "aab"),
%    ["b"] = btrie:pattern_parse("aa*b", "aabb"),
%    ["b", "b"] = btrie:pattern_parse("aa*a*", "aabab"),
%    ["b", "bb"] = btrie:pattern_parse("aa*a*", "aababb"),
%    ["bb", "b"] = btrie:pattern_parse("aa*a*", "aabbab"),
%    ["bb", "bb"] = btrie:pattern_parse("aa*a*", "aabbabb"),
%    error = btrie:pattern_parse("aa*a*", "aaabb"),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

new_instance_state(<<H:8,T/binary>>, V1, V0)
    when is_integer(H) ->
    {{H, H, {{T, V1}}}, V0}.

%% make a new tuple with arity N and default D, then
%% move tuple T into the new tuple at index I
tuple_move(I, N, T, D)
    when is_integer(I), is_integer(N), is_tuple(T),
         (N - I + 1) >= tuple_size(T) ->
    tuple_move_i(I, 1, I + tuple_size(T), erlang:make_tuple(N, D), T).

tuple_move_i(N1, _, N1, T1, _) ->
    T1;

tuple_move_i(I1, I0, N1, T1, T0) ->
    tuple_move_i(I1 + 1, I0 + 1, N1,
        erlang:setelement(I1, T1, erlang:element(I0, T0)), T0).

%wildcard_match_lists_element(_, []) ->
%    error;
%
%wildcard_match_lists_element(_, [$* | _]) ->
%    erlang:exit(badarg);
%
%wildcard_match_lists_element(C, [C | L]) ->
%    {ok, L};
%
%wildcard_match_lists_element(C, [_ | L]) ->
%    wildcard_match_lists_element(C, L).
%
%wildcard_match_lists_valid([], Result) ->
%    Result;
%
%wildcard_match_lists_valid([$* | _], _) ->
%    erlang:exit(badarg);
%
%wildcard_match_lists_valid([_ | L], Result) ->
%    wildcard_match_lists_valid(L, Result).
%
%wildcard_match_lists([], []) ->
%    true;
%
%wildcard_match_lists([], [_ | _] = L) ->
%    wildcard_match_lists_valid(L, false);
%
%wildcard_match_lists([_ | _], [$* | _]) ->
%    erlang:exit(badarg);
%
%wildcard_match_lists([$*], [_ | L]) ->
%    wildcard_match_lists_valid(L, true);
%
%wildcard_match_lists([$*, C | Match], [_ | L]) ->
%    true = C =/= $*,
%    case wildcard_match_lists_element(C, L) of
%        {ok, NewL} ->
%            wildcard_match_lists(Match, NewL);
%        error ->
%            wildcard_match_lists_valid(L, false)
%    end;
%
%wildcard_match_lists([C | Match], [C | L]) ->
%    wildcard_match_lists(Match, L);
%
%wildcard_match_lists(_, L) ->
%    wildcard_match_lists_valid(L, false).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

internal_test_() ->
    [
        {"internal tests", ?_assertEqual(ok, test())}
    ].

% do not have a good way yet to have the proper test switch to use binaries
%proper_test_() ->
%    {timeout, 600, [
%        {"proper tests", ?_assert(trie_proper:qc_run(?MODULE))}
%    ]}.

-endif.

