(*
2-----An F# list can be thought of as representing a set, where the order of the elements in the list is irrelevant.
Write an F# function powerset such that powerset set returns the set of all subsets of set. For example,
  > powerset [1;2;3];;
  val it : int list list
  = [[]; [3]; [2]; [2; 3]; [1]; [1; 3]; [1; 2]; [1; 2; 3]]
*)
//Using List.collect(fun x->[x;1::x]) [[2;3]];;
/// A variant of List.map. (Actually built-in as List.collect.)
let rec appmap f = function
| []    -> []
| x::xs -> f x @ appmap f xs

let rec powerset = 
  function
  | [] -> [[]]
  | y::ys -> appmap (fun x -> [x; y::x]) (powerset ys)













  
/// Insert a value into a list in all possible ways.
let rec insert x = function
| []    -> [[x]]
| y::ys -> (x::y::ys) :: List.map (fun zs -> y::zs) (insert x ys)

/// Find all permutations of a list of distinct elements.
let rec permute = function
| []    -> [[]]
| x::xs -> appmap (insert x) (permute xs)