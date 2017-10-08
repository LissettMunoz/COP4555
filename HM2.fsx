//#1 - Done. Build a cartesian product
let rec cartesian = function
  | ([], []) -> []
  | (_, []) -> []
  | ([], _) -> []
  | (a::[], head::tail) -> (a, head) :: (cartesian([a], tail))
  | (x::xs, y::ys) -> (x, y) :: (cartesian([x], ys)) @ (cartesian(xs, y::ys))

//#2 - Fail
let rec powerset = function
  | ([]) -> [[]]
  | (x:int) -> [x]
  | ((head : int) :: (tail : int list)) -> (powerset(head)) :: (powerset(tail))

//#3 - Done. Transpose a matrix of m x n
//Requires remove_first
let rec transpose = function
    | [] -> []
    | list -> (List.map (List.head) list) :: if (List.length (List.head list)) > 1 then (transpose (List.map (remove_first) list)) else [] //Just need to remove first element of each list on this recursive call

//Removes the first element from any list
let remove_first = function
    | [] -> []
    | x::xs -> xs

//Sample for sanity check
let rec concat = function
    | ([], []) -> []
    | (x::xs, y::ys) -> x :: y :: (concat(xs, ys))