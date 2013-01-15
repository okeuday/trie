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
%%% This file contains trie functions utilized by both the string
%%% (list of integers) trie implementation and the binary trie
%%% implementation.
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

-ifdef(MODE_LIST).
-define(TYPE_NAME, string).
-define(TYPE_EMPTY, []).
-define(TYPE_CHECK(V), is_list(V)).
-define(TYPE_H0T0, [H | T]).
-define(TYPE_H1T1, [H1 | T1]).
-define(TYPE_KEYCHAR, Key ++ [Character]).
-define(TYPE_NEWKEYNODE, NewKey ++ Node).
-else.
-ifdef(MODE_BINARY).
-define(TYPE_NAME, binary).
-define(TYPE_EMPTY, <<>>).
-define(TYPE_CHECK(V), is_binary(V)).
-define(TYPE_H0T0, <<H:8,T/binary>>).
-define(TYPE_H1T1, <<H1:8,T1/binary>>).
-define(TYPE_KEYCHAR, <<Key/binary,Character:8>>).
-define(TYPE_NEWKEYNODE, <<NewKey/binary,Node/binary>>).
-endif.
-endif.

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-type trie_return() :: {integer(), integer(), tuple()}.
-type trie() :: ?TYPE_EMPTY | trie_return().
-export_type([trie/0]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Append a value as a list element in a trie instance.===
%% @end
%%-------------------------------------------------------------------------

-spec append(Key :: ?TYPE_NAME(),
             Value :: any(),
             Node :: trie()) -> trie_return().

append(Key, Value, Node) ->
    ValueList = [Value],
    update(Key, fun(OldValue) -> OldValue ++ ValueList end, ValueList, Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Append a list of values as a list element in a trie instance.===
%% @end
%%-------------------------------------------------------------------------

-spec append_list(Key :: ?TYPE_NAME(),
                  ValueList :: list(),
                  Node :: trie()) -> trie_return().

append_list(Key, ValueList, Node) ->
    update(Key, fun(OldValue) -> OldValue ++ ValueList end, ValueList, Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Erase a value in a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec erase(Key :: ?TYPE_NAME(),
            Node :: trie()) -> trie().

erase(_, ?TYPE_EMPTY = Node) ->
    Node;

erase(?TYPE_H0T0, Node) ->
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
            ?TYPE_H1T1 = T,
            {I0, I1, erlang:setelement(I, Data,
                {erase_node(H1, T1, Node), Value})}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Fetch a value from a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec fetch(?TYPE_NAME(),
            trie_return()) -> any().

fetch(?TYPE_H0T0, {_, _, _} = Node) ->
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
        ?TYPE_H1T1 ->
            case Node of
                {_, _, _} ->
                    fetch_node(H1, T1, Node);
                T when Value =/= error ->
                    Value
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Fetch all the keys in a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec fetch_keys(Node :: trie()) -> list(?TYPE_NAME()).

fetch_keys(Node) ->
    foldr(fun(Key, _, L) -> [Key | L] end, [], Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Fetch the keys within a trie that share a common prefix.===
%% @end
%%-------------------------------------------------------------------------

-spec fetch_keys_similar(Similar :: ?TYPE_NAME(),
                         Node :: trie()) -> list(?TYPE_NAME()).

fetch_keys_similar(Similar, Node) ->
    foldr_similar(Similar, fun(Key, _, L) -> [Key | L] end, [], Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Filter a trie with a predicate function.===
%% @end
%%-------------------------------------------------------------------------

-spec filter(F :: fun((?TYPE_NAME(), any()) -> boolean()),
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
                    case F(?TYPE_KEYCHAR, Value) of
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
                {filter_node(F, ?TYPE_KEYCHAR, Node), Value}));
        true ->
            NewKey = ?TYPE_KEYCHAR,
            if
                ?TYPE_CHECK(Node) ->
                    case F(?TYPE_NEWKEYNODE, Value) of
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

-spec find(?TYPE_NAME(), trie()) -> {ok, any()} | 'error'.

find(_, ?TYPE_EMPTY) ->
    error;

find(?TYPE_H0T0, {_, _, _} = Node) ->
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
            ?TYPE_H1T1 = T,
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

-spec fold(F :: fun((?TYPE_NAME(), any(), any()) -> any()),
           A :: any(),
           Node :: trie()) -> any().

fold(F, A, Node) when is_function(F, 3) ->
    foldl(F, A, Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Fold a function over the keys within a trie that share a common prefix.===
%% Traverses in alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec fold_similar(Similar :: ?TYPE_NAME(),
                   F :: fun((?TYPE_NAME(), any(), any()) -> any()),
                   A :: any(),
                   Node :: trie()) -> any().

fold_similar(Similar, F, A, Node) ->
    foldl_similar(Similar, F, A, Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a trie from a list.===
%% @end
%%-------------------------------------------------------------------------

-spec from_list(list()) -> trie().

from_list(L) ->
    new(L).

%%-------------------------------------------------------------------------
%% @doc
%% ===Merge two trie instance.===
%% Update the second trie parameter with all of the elements
%% found within the first trie parameter.
%% @end
%%-------------------------------------------------------------------------

-spec merge(F :: fun((?TYPE_NAME(), any(), any()) -> any()),
            Node1 :: trie(),
            Node2 :: trie()) -> trie().

merge(F, Node1, ?TYPE_EMPTY) when is_function(F, 3) ->
    Node1;

merge(F, ?TYPE_EMPTY, Node2) when is_function(F, 3) ->
    Node2;

merge(F, Node1, Node2) when is_function(F, 3) ->
    fold(fun (Key, V1, Node) ->
            update(Key, fun (V2) -> F(Key, V1, V2) end, V1, Node)
         end, Node2, Node1).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a new trie instance.===
%% @end
%%-------------------------------------------------------------------------

-spec new() -> ?TYPE_EMPTY.

new() ->
    ?TYPE_EMPTY.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a new trie instance from a list.===
%% The list may contain either: strings, 2 element tuples with a string as the
%% first tuple element, or tuples with more than 2 elements (including records)
%% with a string as the first element (second element if it is a record).
%% If a list of records (or tuples larger than 2 elements) is provided,
%% the whole record/tuple is stored as the value.
%% @end
%%-------------------------------------------------------------------------

-spec new(L :: list()) -> trie().

new(L) ->
    new_instance(L, new()).

new_instance([], Node) ->
    Node;

new_instance([{Key, Value} | T], Node) ->
    new_instance(T, store(Key, Value, Node));

new_instance([Tuple | T], Node)
    when is_tuple(Tuple) ->
    FirstElement = erlang:element(1, Tuple),
    Key = if
        is_atom(FirstElement) ->
            erlang:element(2, Tuple);
        true ->
            FirstElement
    end,
    new_instance(T, store(Key, Tuple, Node));

new_instance([Key | T], Node) ->
    new_instance(T, store(Key, Node)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Insert a value as the first list element in a trie instance.===
%% The reverse of append/3.
%% @end
%%-------------------------------------------------------------------------

-spec prefix(Key :: ?TYPE_NAME(),
             Value :: any(),
             Node :: trie()) -> trie_return().

prefix(Key, Value, Node) ->
    update(Key, fun(OldValue) -> [Value | OldValue] end, [Value], Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Size of a trie instance.===
%% @end
%%-------------------------------------------------------------------------

-spec size(Node :: trie()) -> non_neg_integer().

size(Node) ->
    fold(fun(_, _, I) -> I + 1 end, 0, Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store only a key in a trie instance.===
%% @end
%%-------------------------------------------------------------------------

-spec store(Key :: ?TYPE_NAME(),
            Node :: trie()) -> trie_return().

store(Key, Node) ->
    store(Key, empty, Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert all entries in a trie to a list.===
%% The list is in alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec to_list(Node :: trie()) -> list({?TYPE_NAME(), any()}).

to_list(Node) ->
    foldr(fun (Key, Value, L) -> [{Key, Value} | L] end, [], Node).
        
%%-------------------------------------------------------------------------
%% @doc
%% ===Update a counter in a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec update_counter(Key :: ?TYPE_NAME(),
                     Increment :: number(),
                     Node :: trie()) -> trie_return().

update_counter(Key, Increment, Node) ->
    update(Key, fun(I) -> I + Increment end, Increment, Node).

