-module(avl_tree).

%% API
-export([
  new_node/2,
  insert/3
]).

%%%===================================================================
%%% Types
%%%===================================================================

%% AVL Tree Node Definition
-type t() :: {
  Key    :: any(),
  Value  :: any(),
  Height :: integer(),
  Left   :: t(),
  Right  :: t()
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
  {Key, Value, 1, nil, nil}.

-spec insert(Key, Value, Tree1) -> Tree2 when
  Key   :: any(),
  Value :: any(),
  Tree1 :: t(),
  Tree2 :: t().
insert(Key, Value, nil) ->
  new_node(Key, Value);
insert(Key, Value, {NodeKey, NodeVal, _, Left, Right}) when Key < NodeKey ->
  NewLeft = insert(Key, Value, Left),
  Height = 1 + max(height(NewLeft), height(Right)),
  Node = {NodeKey, NodeVal, Height, NewLeft, Right},
  maybe_rotate(balance_factor(Node), Key, Node);
insert(Key, Value, {NodeKey, NodeVal, _, Left, Right}) when Key >= NodeKey ->
  NewRight = insert(Key, Value, Right),
  Height = 1 + max(height(Left), height(NewRight)),
  Node = {NodeKey, NodeVal, Height, Left, NewRight},
  maybe_rotate(balance_factor(Node), Key, Node).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
maybe_rotate(Balance, Key, {_, _, _, {K, _, _, _, _}, _} = Node) when Balance > 1, Key < K ->
  right_rotate(Node);
maybe_rotate(Balance, Key, {_, _, _, _, {K, _, _, _, _}} = Node) when Balance < -1, Key > K ->
  left_rotate(Node);
maybe_rotate(Balance, Key, {PK, PV, PH, {K, _, _, _, _} = Left, Right}) when Balance > 1, Key > K ->
  right_rotate({PK, PV, PH, left_rotate(Left), Right});
maybe_rotate(Balance, Key, {PK, PV, PH, Left, {K, _, _, _, _} = Right}) when Balance < -1, Key < K ->
  left_rotate({PK, PV, PH, Left, right_rotate(Right)});
maybe_rotate(_, _, Node) ->
  Node.

%% @private
right_rotate({ParentKey, ParentVal, _, {K, V, _, LTree, RTree}, Right}) ->
  RightHeight = max(height(RTree), height(Right)) + 1,
  NewRight = {ParentKey, ParentVal, RightHeight, RTree, Right},
  ParentHeight = max(height(LTree), height(NewRight)) + 1,
  {K, V, ParentHeight, LTree, NewRight};
right_rotate(Node) ->
  Node.

%% @private
left_rotate({ParentKey, ParentVal, _, Left, {K, V, _, LTree, RTree}}) ->
  LeftHeight = max(height(Left), height(LTree)) + 1,
  NewLeft = {ParentKey, ParentVal, LeftHeight, Left, LTree},
  ParentHeight = max(height(NewLeft), height(RTree)) + 1,
  {K, V, ParentHeight, NewLeft, RTree};
left_rotate(Node) ->
  Node.

%% @private
height(nil) ->
  0;
height({_, _, Height, _, _}) ->
  Height.

%% @private
balance_factor(nil) ->
  0;
balance_factor({_, _, _, Left, Right}) ->
  height(Left) - height(Right).
