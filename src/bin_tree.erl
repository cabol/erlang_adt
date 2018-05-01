-module(bin_tree).

-export([
  new_node/2,
  insert/3,
  lookup/2,
  preorder/1,
  preorder/2,
  inorder/1,
  inorder/2
]).

%%%===================================================================
%%% Types
%%%===================================================================

%% Binary Tree Node Definition
-type t() :: {
  Key   :: any(),
  Value :: any(),
  Left  :: t(),
  Right :: t()
}.

-export_type([t/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new_node(Key, Value) -> Tree when
  Key   :: any(),
  Value :: any(),
  Tree  :: t().
new_node(Key, Value) ->
  {Key, Value, nil, nil}.

-spec insert(Key, Value, Tree1) -> Tree2 when
  Key   :: any(),
  Value :: any(),
  Tree1 :: t(),
  Tree2 :: t().
insert(Key, Value, nil) ->
  new_node(Key, Value);
insert(Key, Value, {NodeKey, NodeVal, Left, Right}) when Key < NodeKey ->
  {NodeKey, NodeVal, insert(Key, Value, Left), Right};
insert(Key, Value, {NodeKey, NodeVal, Left, Right}) when Key >= NodeKey ->
  {NodeKey, NodeVal, Left, insert(Key, Value, Right)}.

-spec lookup(Key, Tree) -> Value when
  Key   :: any(),
  Tree  :: t(),
  Value :: any().
lookup(_Key, nil) ->
  nil;
lookup(Key, {Key, Val, _, _}) ->
  Val;
lookup(Key, {NodeKey, _, Left, _Right}) when Key < NodeKey ->
  lookup(Key, Left);
lookup(Key, {NodeKey, _, _Left, Right}) when Key >= NodeKey ->
  lookup(Key, Right).

%% @equiv preorder(fun(K, _) -> K end, Tree)
preorder(Tree) ->
  preorder(fun(K, _) -> K end, Tree).

-spec preorder(Fun, Tree) -> Res when
  Fun  :: fun((any(), any()) -> any()),
  Tree :: t(),
  Res  :: any().
preorder(Fun, Tree) ->
  preorder(Fun, Tree, []).

%% @private
preorder(_, nil, Acc) ->
  lists:reverse(Acc);
preorder(Fun, {Key, Val, Left, Right}, Acc) ->
  Result = [
    Fun(Key, Val),
    lists:flatten(preorder(Fun, Left)),
    lists:flatten(preorder(Fun, Right))
  ],
  [Result | Acc].

%% @equiv inorder(fun(K, _) -> K end, Tree)
inorder(Tree) ->
  inorder(fun(K, _) -> K end, Tree).

-spec inorder(Fun, Tree) -> Res when
  Fun  :: fun((any(), any()) -> any()),
  Tree :: t(),
  Res  :: any().
inorder(Fun, Tree) ->
  inorder(Fun, Tree, []).

%% @private
inorder(_, nil, Acc) ->
  lists:reverse(Acc);
inorder(Fun, {Key, Val, Left, Right}, Acc) ->
  Result = [
    lists:flatten(inorder(Fun, Left)),
    Fun(Key, Val),
    lists:flatten(inorder(Fun, Right))
  ],
  [Result | Acc].
