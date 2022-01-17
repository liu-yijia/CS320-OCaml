
(* the type of a plymorphic tree *)
type 'a tree =
  | Leaf of 'a 
  | Node of 'a tree * 'a tree

(* map function for trees:
Example:
map_tree (fun x -> x+1) (Node (Leaf 1, Leaf 2)) =  (Node (Leaf 2, Leaf 3))
map_tree (fun _ -> 0)  (Node (Node (Leaf true, Node (Leaf true, Leaf false)), Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false))) =
                       (Node (Node (Leaf 0   , Node (Leaf 0   , Leaf 0    )), Node (Node (Leaf 0   , Node (Leaf 0   , Leaf 0    )), Leaf 0    ))) *)
let rec map_tree (f: 'a -> 'b) (tree: 'a tree): 'b tree = 
  match tree with 
    Leaf x -> Leaf (f x)
  |
    Node (left,right) -> Node (map_tree f left, map_tree f right)

(* fold function for trees:
Example:
fold_tree ( * ) (fun x -> x) (Node (Leaf 3, Leaf 2)) = 6
fold_tree (+) (fun _ -> 1) (Node (Node (Leaf true, Node (Leaf true, Leaf false)), Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false))) = 7 *)
let rec fold_tree (node: 'b -> 'b -> 'b) (leaf: 'a -> 'b) (tree: 'a tree): 'b  = 
  match tree with
    Leaf x -> leaf x
  |
    Node (left,right) -> node (fold_tree node leaf left) (fold_tree node leaf right)

(* sum the contents of an int tree
Example:
sum_ints (Node (Leaf 1, Leaf 2)) = 3 *)
let rec sum_ints (tree: int tree): int = 
  match tree with 
    Leaf x -> x
  |
    Node (left,right) -> sum_ints left + sum_ints right

(* find the size of the tree
Example:
tree_size (Leaf 1) = 1
tree_size (Node (Leaf 1, Leaf 2)) = 3 *)
let rec tree_size (tree: 'a tree): int = 
  match tree with
    Leaf x -> 1
  |
    Node (left,right) -> tree_size left + tree_size right + 1

(* find the height of the tree
Example:
tree_height (Leaf 2) = 1
tree_height (Node ((Node (Leaf 1, (Node ((Node (Leaf 1, Leaf 2)), Leaf 2)))), Leaf 2)) = 5 *)
let rec tree_height (tree: 'a tree): int = 
  match tree with
    Leaf x -> 1
  |
    Node (left,right) -> max (tree_height left) (tree_height right) + 1

(* a function that takes a predicate on trees and retuns true if any subtree satisfies that predicate
Example:
tree_contains (Node (Leaf 1, Leaf 2)) (fun x -> match x with Leaf 2 -> true | _ -> false) = true
tree_contains (Node (Leaf 1, (Node ((Node (Leaf 1, Leaf 2)), Leaf 2)))) (fun x -> tree_height x > 2) = true *)
let rec tree_contains (tree: 'a tree) (look_for: 'a tree -> bool): bool = 
  if look_for tree = true then true
  else 
    match tree with 
      Leaf x -> false 
    |
      Node (left,right) -> (tree_contains left look_for) || (tree_contains right look_for)

(* a function that shows bool trees :

For example,
show_bool_tree (Leaf true) ="true"
show_bool_tree (Node (Leaf true, Leaf false)) = "(true^false)" 
show_bool_tree  (Node (Node (Leaf true, Node (Leaf true, Leaf false)),
   Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false))) =
    "((true^(true^false))^((true^(true^false))^false))" 
*)
let rec show_bool_tree (tree: bool tree) : string = 
  match tree with 
    Leaf x -> Bool.to_string x
  |
    Node (left,right) -> "(" ^ (show_bool_tree left) ^ "^" ^ (show_bool_tree right) ^ ")"


(* standard functions to convert between string and char list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let string_of_char c = String.make 1 c

let rec implode (chars: char list) : string =
  match chars with
    [] -> ""
  | h::t ->  string_of_char h ^ (implode t)


(* write a fubction that reads bool trees : for all (finite) t : bool trees.read_bool_tree t = Some (show_bool_tree t)
Example:
read_bool_tree "true" = Some (Leaf true)
read_bool_tree "false" = Some (Leaf false)
read_bool_tree "tralse" = None
read_bool_tree "(true^false)" = Some (Node (Leaf true, Leaf false))
read_bool_tree "((true^(true^false))^((true^(true^false))^false))" =
Some
 (Node (Node (Leaf true, Node (Leaf true, Leaf false)),
   Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false))) *)

let rec read_bool_prefix (l: char list) : ((bool * (char list)) option) =
  match l with
  't'::'r'::'u'::'e'::rest -> Some (true, rest)
  |
    'f'::'a'::'l'::'s'::'e'::rest -> Some (false, rest)
  |
    _ -> None

let rec read_bool_tree_prefix (l : char list) : ((bool tree * (char list)) option) = 
  match l with
    [] -> None
  |
    '('::rest -> (match (read_bool_tree_prefix rest) with
        None -> None
      |
        Some (left1, rest1) -> match rest1 with
          [] -> None
        |
          '^'::t2 -> (match (read_bool_tree_prefix t2) with
              None -> None
            |
              Some (left2,rest2) -> (match rest2 with
                  ')'::t3 -> Some (Node (left1,left2),t3)
                |
                  _ -> Some (Node (left1,left2),rest2)))
        |
          ')'::t1 -> Some (left1, t1)
        |
          _ -> None)
  |
    _ -> match (read_bool_prefix l) with
      None -> None
    |
      Some (left3, rest3) -> Some (Leaf left3, rest3)

let rec read_bool_tree (tree: string) : ((bool tree) option) = 
  match (read_bool_tree_prefix (explode tree)) with
    Some (tree,[]) -> Some tree
  |
    _ -> None

(* a fubction that checks that parenthisis are balnaced:
Parenthisis are balenced if there are no parenthises
Parenthisis are balenced if ( and )  enclose a balenced parenthises
Parenthisis are balenced if balenced parenthises are ajacent to a balenced parenthisis
Example:
matching_parens "" = true
matching_parens "((((((((((()))))))))))" = true
matching_parens "()()()()()()" = true
matching_parens "(()())" = true
matching_parens "())(()" = false *)

let rec matching_parens (tree: string) : bool = 
  let rec aux tree accum = 
    match tree with
      [] -> List.length accum = 0
    |
      '('::rest -> aux rest ('('::accum)
    |
      ')'::rest -> (match accum with 
          '('::rest1 -> aux rest rest1
        |
          _ -> false)
    |
      _ -> false
  in aux (explode tree) []
