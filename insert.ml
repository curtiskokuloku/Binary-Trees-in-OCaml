(* Construct a binary search tree from a list of integer numbers. *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree


let rec insert tree x = match tree with
    | Empty -> Node (x, Empty, Empty)
    | Node (y, l, r) ->
       if x = y then tree
       else if x < y then Node (y, insert l x, r)
       else Node (y, l, insert r x)
  let construct l = List.fold_left insert Empty l