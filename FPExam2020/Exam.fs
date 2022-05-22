module Exam2020
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but
   it does allow you to work in interactive mode and you can just remove the '='
   to make the project compile work again.

   Do not remove the line (even though that does work) because you may inadvertantly
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode.

   Alternative, keep the line as is, but load ExamInteractive.fsx into the interactive environment
   *)
(* module Exam2020 = *)

(* 1: Insertion sort *)

(* Question 1.1 *)

let rec insert el lst =
    match lst with
    | [] -> [ el ]
    | x :: xs when x >= el -> el :: lst
    | x :: xs when x < el -> x :: insert el xs
(*el :: insert x xs*)

(* let  insertionSort lst =
        match lst with
        |[] -> []
        |_ -> List.fold (fun el acc -> insert el acc) [] lst*)
let rec insertionSort lst =
    match lst with
    | [] -> []
    | x :: xs when x > xs.Head && xs.Length > 0 -> xs.Head :: x :: insertionSort xs
    | x :: xs when x <= xs.Head && xs.Length > 0 -> x :: xs.Head :: insertionSort xs
    |_ -> lst 







(* Question 1.2 *)

let insertTail _ = failwith "not implemented"
let insertionSortTail _ = failwith "not implemented"

(* Question 1.3 *)

(*
    Q: Why are the higher-order functions from the List library
    not a good fit to implement insert?

    A: <Your answer goes here>
    *)

let insertionSort2 _ = failwith "not implemented"

(* Question 1.4 *)

let insertBy _ = failwith "not implemented"
let insertionSortBy _ = failwith "not implemented"

(* 2: Code Comprehension *)
//type = 'a -> 'a list -> 'a list
//this is a remove function. Removes all elements that are equal to x
//name : remove
let rec foo x =
    function
    | y :: ys when x = y -> ys
    | y :: ys -> y :: (foo x ys)

//type 'a -> list<list<'a>> -> list<list<'a>>
//adds element x to the start of every list in the outer list
//name : addToListOfLists
let rec bar x =
    function
    | [] -> []
    | xs :: xss -> (x :: xs) :: bar x xss



//type list<'a> -> list<list<'a>>
//creaes all possible permutations of a list
//name: generatePermutations
let rec baz =
    function
    | [] -> []
    | [ x ] -> [ [ x ] ]
    | xs ->
        let rec aux =
            function
            | [] -> []
            | y :: ys -> ((foo y >> baz >> bar y) xs) @ (aux ys)

        aux xs
   
 
(* Question 2.1 *)

(*

    Q: What are the types of functions foo,  bar, and baz?

    A: <Your answer goes here>


    Q: What do functions foo, bar, and baz do?
       Focus on what they do rather than how they do it.

    A: <Your answer goes here>


    Q: What would be appropriate names for functions
       foo, bar, and baz?

    A: <Your answer goes here>

    *)


(* Question 2.2 *)


(*
    The function foo generates a warning during compilation:
    Warning: Incomplete pattern matches on this expression.


    Q: Why does this happen, and where?

    A: It happens because there is not match-case for the empty list


    Q: For these particular three functions will this incomplete
       pattern match ever cause problems for any possible execution of baz?
       If yes, why; if no, why not.

    A: No since baz already takes into account empty lists

    *)

let foo2 x =
    function
    | [] -> []
    | y :: ys when x = y -> ys
    | y :: ys -> y :: (foo x ys)

(* Question 2.3 *)

(*
    In the function baz there is a sub expression foo y >> baz >> bar y

    Q: What is the type of this expression

    A: list<'a> -> list<list<'a>>


    Q: What does it do? Focus on what it does rather than how it does it.

    A: It takes a list, removes the value y from the list. Baz is then called with that list as an argument, where all permutations of that list is generated.
           Then y is added to each list again at the front of each list

    *)

(* Question 2.4 *)

let bar2 el lst = List.fold (fun acc x ->  List.insertAt (List.length acc)  (List.insertAt 0 el x) acc) [] lst

(* Question 2.5 *)

let baz2 lst =
     List.fold (fun acc _ -> (List.permute (fun i -> (i+(List.length acc)+1) % (List.length lst)) lst) :: acc) [] lst
        
        
        
        
        (*let rec aux =
            function
            | [] -> []
            | y :: ys -> ((foo y >> baz >> bar y) xs) @ (aux ys)

        aux xs*)

(* Question 2.6 *)

(*

    Q: The function foo is not tail recursive. Why?

    A: <Your answer goes here>

    *)

let fooTail el lst =
    let rec fooTailC lst c =
        match lst with
        | [] -> c []
        | y :: ys when el = y -> c ys
        | y :: ys -> fooTailC ys (fun ls -> c (y :: ls))
    fooTailC lst id
(* 3: Rock Paper Scissors *)

(* Question 3.1 *)

type shape =
       |Rock
       |Paper
       |Scissors
type result =
    |PlayerOneWin
    |PlayerTwoWin
    |Draw(* replace unit with the correct type declaration *)

let rps s1 s2 =
    match s1 with
    |s1 when s1 = s2 -> Draw
    |s1 when s1 = Rock && s2 = Scissors -> PlayerOneWin
    |s1 when s1 = Scissors && s2 = Paper -> PlayerOneWin
    |s1 when s1 = Paper && s2= Rock -> PlayerOneWin
    |_ -> PlayerTwoWin

(* Question 3.2 *)

type strategy = (shape * shape) list -> shape

let parrot (sh: shape) lst  =
    match lst with
    |[] -> sh
    |_ ->  snd (List.head lst) 

let beatingStrat lst =
    let opponentMoves = List.fold (fun acc x -> (snd x) :: acc) [] lst
    let summationOfMoves = List.countBy id opponentMoves
    let sortedList =List.sortBy (fun (_, y) -> -y) summationOfMoves
    match sortedList with
    |[] -> Rock
    |x :: y :: z :: _ when (snd x) = (snd y) && (snd y) = (snd z) -> Rock
    |x :: y :: _ when (snd x) = (snd y) ->
        match ( fst x), (fst y) with
        |Rock, Scissors -> Rock
        |Rock, Paper -> Paper
        |Scissors, Rock -> Rock
        |Scissors, Paper -> Rock
        |Paper, Rock -> Paper
        |Paper, Scissors-> Rock
    | x :: _->
        match (fst x) with
        |Rock -> Paper
        |Scissors -> Rock
        |Paper -> Scissors


        
    
    
    
  

let roundRobin _ = failwith "not implemented"

(* Question 3.3 *)

(*

    Q: It may be tempting to generate a function that calculates your
       point tuple after n rounds and then use Seq.initInfinite to
       generate the sequence. This is not a good solution. Why?

    A: <Your answer goes here>

    *)

let bestOutOf _ = failwith "not implemented"

(* Question 3.4 *)

let playTournament _ = failwith "not implemented"

(* 4: Revers Polish Notation *)

(* Question 4.1 *)

type stack = L of list<int> (* replace unit with the correct type declaration *)

let emptyStack =
    L List.empty 

(* Question 4.2 *)

type SM<'a> = S of (stack -> ('a * stack) option)

let ret x = S(fun s -> Some(x, s))
let fail = S(fun _ -> None)

let bind f (S a) : SM<'b> =
    S
        (fun s ->
            match a s with
            | Some (x, s') ->
                let (S g) = f x
                g s'
            | None -> None)

let (>>=) x f = bind f x
let (>>>=) x y = x >>= (fun _ -> y)

let evalSM (S f) = f emptyStack

let push x (L st) = ret (L (List.insertAt 0 x st))
let pop (L st) = ret (List.head st) 

(* Question 4.3 *)

let write str : SM<unit> =
    S
        (fun s ->
            printf "%s" str
            Some((), s))

let read =
    let rec aux acc =
        match System.Console.Read() |> char with
        | '\n' when acc = [] -> None
        | c when System.Char.IsWhiteSpace c ->
            acc
            |> List.fold (fun strAcc ch -> (string ch) + strAcc) ""
            |> Some
        | c -> aux (c :: acc)

    S(fun s -> Some(aux [], s))

(*

    Q: Consider the definition of write There is a reason that the definition
       is S (fun s -> printf "%s" str; Some ((), s)) and not just
       ret (printf "%s" str). For a similar reason, in read, we write
       S (fun s -> Some (aux [], s)) and not ret (aux []).
       What is the problem with using ret in both of these cases?

    A: <Your answer goes here>

    *)

(* Question 4.4 *)

(* You may solve this exercise either using monadic operators or
        using computational expressions. *)

type StateBuilder() =

    member this.Bind(f, x) = bind x f
    member this.Return(x) = ret x
    member this.ReturnFrom(x) = x
    member this.Combine(a, b) = a >>= (fun _ -> b)

let state = new StateBuilder()

let calculateRPN _ = failwith "not implemented"
