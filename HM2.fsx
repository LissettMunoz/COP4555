#Build a cartesian product
let rec cartesian = function
  | ([], []) -> []
  | (_, []) -> []
  | ([], _) -> []
  | (a::[], head::tail) -> (a, head) :: (cartesian([a], tail))
  | (x::xs, y::ys) -> (x, y) :: (cartesian([x], ys)) @ (cartesian(xs, y::ys))


#Sample for sanity check
let rec concat = function
    | ([], []) -> []
    | (x::xs, y::ys) -> x :: y :: (concat(xs, ys))