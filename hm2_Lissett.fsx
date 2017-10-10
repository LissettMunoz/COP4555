(*UPDATED VERSION*)

(*1. 
This F# function was created with the assistance of Professor Geoffrey Smith.
cartesian (xs, ys) is uncurried and takes as input two lists xs and ys and
returns them as a list of pairs that represents the Cartesian product of xs and ys. 

The base case checks to see if the first list is empty, regardless of the
contents of the second list. If it is empty it returns an empty list since
cartesian needs to return a pair that represents the product of xs and ys. 

The code for the non-base case shortens the list using cartesian(xs, ys)
and uses maps to create a pair with the head of x and the head of y. 

The recursive call of the input does become smaller than the original input.
xs is smaller than x::xs.
*)

let rec cartesian = function
| ([],ys) -> []
| (x::xs, ys) -> List.map (fun y -> (x,y)) ys @ cartesian (xs, ys) ;;

2. 

3.

(*4. 

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

 
(*5.

1. 

Rule 1: There is a circumstance in which the base case fails to return the correct 
result for the input. There is no base case available for when there is only one item 
in the list. There is only a base case for when the list is empty. 

Rule 2: There is no circumstance in which the code for a non-base case can fail
to transform correct results returned by the recursive calls. The non-base case
correctly splits the list in placing the first half into M and second half into N
using split. From there it recusively splits each half in half further and merges
the results using merge, placing them in numerical order. 

Rule 3: There is no circumstance in which the definition can make a recursive call on an
input that's not smaller than the original input. L is continously cut in half and split
into M and N, which are each then recursively split in half using mergesort.  

2. F# infers that mergesort has a type 'a list -> 'b list. Which doesn't make sense since
the items on the list do not change, just the order of the list. 

3. The program was missing a base case, it was missing the case where the list only
has one item. With that base case added F# infers that mergesort has a type
'a list -> 'a list.

let rec mergesort = function
  | []  -> []
  | [x] -> [x]
  | L   -> let (M, N) = split L
           merge (mergesort M, mergesort N)   

*)
6.
