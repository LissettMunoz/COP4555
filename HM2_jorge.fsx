(*
1. 
*)
let rec cartesian = function
| ([],ys) -> []
| (x::xs, ys) -> List.map (fun y -> (x,y)) ys @ cartesian (xs, ys) ;;





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


(*
4. 
Rule 1: There is no circumstance in which the base case fails to return the 
correct result for the input. When the list is empty an empty list is returned
and when there is only one item in the list that one item is returned. 

Rule 2: There is a circumstance in which the code for a non-base case can fail
to transform correct results returned by the recursive calls. It does not sort
the list correctly when the list contains more than two item. It sorts them in 
order by checking if the first two are in order, then the second and third item
and so on. However, this doesn't work since it doesn't check if the previous
items from what it was switched with are in order. 

ex. [2;5;1]
It will check if 2 is smaller or equal to 5, which is correct, so it will remain in place.
[2;5;1]
Now it will check if 5 is smaller or equal to 1 which is incorrect and will then switch
the items. 
[2;1;5]
It is now done, the list is not in order since 1 is smaller than 2.  

Rule 3: There is no circumstance in which the definition can make a recursive call on an
input that's not smaller than the original input. xs is smaller than x1::x2::xs. 
 
*)


(*
Recall that an F# function that takes two arguments can be coded 
in either uncurried form (in which case it takes a pair as its input) or 
curried form (in which case it takes the first argument and returns a function that takes the second argument).
In fact it is easy to convert from one form to the other in F#. To this end, 
define an F# function curry f that converts an uncurried function to a curried function,
and an F# function uncurry f that does the opposite conversion. For example,
  > (+);;
  val it : (int -> int -> int) = <fun:it@13-7>
  > let plus = uncurry (+);;
  val plus : (int * int -> int)
  > plus (2,3);;
  val it : int = 5
  > let cplus = curry plus;;
  val cplus : (int -> int -> int)
  > let plus3 = cplus 3;;
  val plus3 : (int -> int)
  > plus3 10;;
  val it : int = 13


*)
(*
> (+);;
val it : (int -> int -> int) = <fun:it@3-9>

> let plus= uncurry it;;
val plus : (int * int -> int)

> plus(4,5);;
val it : int = 9
*)
let uncurry cFunc (a,b)= cFunc a b   


(*
> let cplus = curry plus;;
val cplus : (int -> int -> int)

> cplus 3;;
val it : (int -> int) = <fun:Invoke@3253>

> it 6;;
val it : int = 9

> 
*)
let curry uFunc a b = uFunc (a,b)





  
/// Insert a value into a list in all possible ways.
let rec insert x = function
| []    -> [[x]]
| y::ys -> (x::y::ys) :: List.map (fun zs -> y::zs) (insert x ys)

/// Find all permutations of a list of distinct elements.
let rec permute = function
| []    -> [[]]
| x::xs -> appmap (insert x) (permute xs)
