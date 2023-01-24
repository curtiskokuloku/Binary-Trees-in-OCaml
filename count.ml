(** Count the leaves of a binary tree. 

    A leaf is a node with no successors. 
    
    Write a function count_leaves to count them. *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree


let rec count_leaves = function
    | Empty -> 0
    | Node (_, Empty, Empty) -> 1
    | Node (_, l, r) -> count_leaves l + count_leaves r