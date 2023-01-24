(** Construct completely balanced binary trees. 
    A binary tree is either empty or it is composed of a root 
    element and two successors, which are binary trees themselves.

    In OCaml, one can define a new type binary_tree that carries 
    an arbitrary value of type 'a (thus is polymorphic) at each 
    node. *)


type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

(* Build all trees with given [left] and [right] subtrees. *)

  let add_trees_with left right all =
    let add_right_tree all l =
      List.fold_left (fun a r -> Node ('x', l, r) :: a) all right in
    List.fold_left add_right_tree all left

  let rec cbal_tree n =
    if n = 0 then [Empty]
    else if n mod 2 = 1 then
      let t = cbal_tree (n / 2) in
      add_trees_with t t []
    else (* n even: n-1 nodes for the left & right subtrees altogether. *)
      let t1 = cbal_tree (n / 2 - 1) in
      let t2 = cbal_tree (n / 2) in
      add_trees_with t1 t2 (add_trees_with t2 t1 [])


(* Construct height-balanced binary trees. *)

(** In a height-balanced binary tree, the following property holds
    for every node: The height of its left subtree and the height 
    of its right subtree are almost equal, which means their 
    difference is not greater than one.

    Write a function hbal_tree to construct height-balanced binary 
    trees for a given height. The function should generate all 
    solutions via backtracking. Put the letter 'x' as information 
    into all nodes of the tree.*)

let rec hbal_tree n =
    if n = 0 then [Empty]
    else if n = 1 then [Node ('x', Empty, Empty)]
    else
    (* [add_trees_with left right trees] is defined in a question above. *)
      let t1 = hbal_tree (n - 1)
      and t2 = hbal_tree (n - 2) in
      add_trees_with t1 t1 (add_trees_with t1 t2 (add_trees_with t2 t1 []))