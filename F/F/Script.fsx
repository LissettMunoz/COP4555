#load "C:/Users/jleyva/projects/F#/F/packages/FSharp.Charting.0.91.1/lib/net45/FSharp.Charting.fsx"


open FSharp.Charting
open System

let linear x = 2.0 * x

let quadratic x = x**2.0

let exp x y = x**y



[ for x in 0.0 .. 3.14 -> (x,sin x) ] |> Chart.Line

let add x = (+) x 
let next = add 1
next  5

//Greater than infix functions 
let (^) x y = x > y

let greater x y = x^y

let list = [9;12;6;14;89]

//Lambda expressions 
List.map (fun x -> x ^ 10) list 
 

//Recursion
let rec length = function 
    | [] -> 0
    | x::xs -> 1 + length xs

length list 
let rec factorial = function 
    | 0 -> 1
    | 1 -> 1
    | n -> n*factorial (n-1)

 //Piping 
let doubleSin n = n |> sin |> (*)2.

//Function composition
let g10 n = n>10
let isFalse n = if n then false else true

let l10 = g10 >> isFalse




