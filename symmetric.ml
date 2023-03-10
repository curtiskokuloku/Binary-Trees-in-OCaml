(** Let us call a binary tree symmetric if you can draw a vertical
    line through the root node and then the right subtree is the 
    mirror image of the left subtree. Write a function 
    is_symmetric to check whether a given binary tree is symmetric. *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let rec is_mirror t1 t2 =
    match t1, t2 with
    | Empty, Empty -> true
    | Node(_, l1, r1), Node(_, l2, r2) ->
       is_mirror l1 r2 && is_mirror r1 l2
    | _ -> false

  let is_symmetric = function
    | Empty -> true
    | Node(_, l, r) -> is_mirror l r