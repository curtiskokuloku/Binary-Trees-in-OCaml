(* A string representation of binary trees. *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let rec string_of_tree = function
    | Empty -> ""
    | Node(data, l, r) ->
       let data = String.make 1 data in
       match l, r with
       | Empty, Empty -> data
       | _, _ -> data ^ "(" ^ (string_of_tree l)
                 ^ "," ^ (string_of_tree r) ^ ")"


(* One can also use a buffer to allocate a lot less memory: *)

let rec buffer_add_tree buf = function
    | Empty -> ()
    | Node (data, l, r) ->
       Buffer.add_char buf data;
       match l, r with
       | Empty, Empty -> ()
       | _, _ -> Buffer.add_char buf '(';
                 buffer_add_tree buf l;
                 Buffer.add_char buf ',';
                 buffer_add_tree buf r;
                 Buffer.add_char buf ')'
                 let string_of_tree t =
    let buf = Buffer.create 128 in
      buffer_add_tree buf t;
      Buffer.contents buf


(** For the reverse conversion, we assume that the string is well
    formed and do not deal with error reporting. *)

let tree_of_string =
    let rec make ofs s =
      if ofs >= String.length s || s.[ofs] = ',' || s.[ofs] = ')' then
        (Empty, ofs)
      else
        let v = s.[ofs] in
        if ofs + 1 < String.length s && s.[ofs + 1] = '(' then
          let l, ofs = make (ofs + 2) s in (* skip "v(" *)
          let r, ofs = make (ofs + 1) s in (* skip "," *)
            (Node (v, l, r), ofs + 1) (* skip ")" *)
        else (Node (v, Empty, Empty), ofs + 1)
    in
      fun s -> fst (make 0 s)