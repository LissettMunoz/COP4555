//1. Define infix operators .+ and .* to do addition and multiplication of fractions:
//The precedences is working fine but I don't know why :(
//Euclid's algorithm for gcd (greatest common divisor)
let rec gcd = function
  | (a,0) -> a
  | (a,b) -> gcd (b, a % b)
  
let lowestF (a,b) =
    let gcdV=gcd (a,b)
    (a/gcdV,b/gcdV)

let (.*) (a,b) (c,d) =
    let divN=a*c
    let divD=b*d
    lowestF(divN,divD)

let (.+) (a,b) (c,d) =
    let sumD=b*d    
    let sumN=sumD/b*a + sumD/d*c
    lowestF(sumN,sumD)
  
 
//2. Revert a list of lists
let revlists l = List.map (fun x -> List.rev x) l

//3. Write an F# function interleave(xs,ys) that interleaves two lists:

let interleave_old (a,b) = 
    let rec interleaveLists  = function
    | ([],[],list) -> list
    | ([],_,_)  -> failwith "Lists need to be equals"
    | (_,[],_)  -> failwith "Lists need to be equals"
    | (a::aa,b::bb,list)  -> interleaveLists (aa,bb,a::b::list)
    
    interleaveLists(List.rev a,List.rev b,[])
///interleave(xs,ys) implementation after taking a look at Rodolfo's shuffle impl...
// > interleave ([1;2;3],[4;5;6]);;
//  val it : int list = [1; 4; 2; 5; 3; 6]
let rec interleave = function 
    | ([],[]) -> []
    | (a::aa,[])  -> a::interleave(aa,[])
    | ([],b::bb)  -> b::interleave([],bb)
    | (a::aa,b::bb)  -> a::b::interleave(aa,bb)



//4. Write an F# function cut xs that cuts a list into two equal parts:

//To implement cut, first define an auxiliary function gencut(n, xs) 
//that cuts xs into two pieces, where n gives the size of the first piece:

//gencut(2, [1;3;4;2;7;0;9]);;
//  val it : int list * int list = ([1; 3], [4; 2; 7; 0; 9])
//> cut [1;2;3;4;5;6];;
//  val it : int list * int list = ([1; 2; 3], [4; 5; 6])
let cut list=
    let gencut(n,list:'a list)=(list.[0..n-1],list.[n..List.length list-1])
    gencut(List.length list /2,list)


//Here are questions 5 and 6. Jorge, question 5 relied on question 4, so I guess you can take it from there, or come up with your own solution. 
//I manually tested question 6 for sizes 4, 6, 8, and it seems to be working fine. 
//If any of you fancy testing with lists of size 10 and 12 for safety, be my guest. 
//I will be out of town this weekend, so if you guys have any question for me I might be able to address it on Sunday.
//shuffle [1;2;3;4;5;6;7;8];;
//val it : int list = [1; 5; 2; 6; 3; 7; 4; 8]
let shuffle (us) =
    let split (xs : 'a list) =  (xs.[0..(List.length xs)/2-1], xs.[(List.length xs)/2..(List.length xs - 1)])
    let rec inner_shuffle = function
    | ([], []) -> []
    | (z::zs, []) -> z::(inner_shuffle (zs, [])) //For safety and to avoid warnings
    | ([], z::zs) -> z::(inner_shuffle (zs, [])) //For safety and to avoid warnings
    | (v::vs, w::ws) -> v::w::(inner_shuffle (vs, ws))
    let (xs,ys) = split us
    inner_shuffle (xs, ys)

let countshuffles n =
    let xs = [1..n]
    let rec countaux = function
    | (xs, ys) -> if ( xs = ys ) then 0 else 1 + countaux(shuffle xs, ys)
    1 + countaux (shuffle xs, xs) //This extra 1 is because I'm shuffling once to start countaux
    
    
    







    