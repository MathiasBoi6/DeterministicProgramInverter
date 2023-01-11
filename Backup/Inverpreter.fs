module InterpretConsLang

open System

type Exp =
    | Case of Exp * Exp * Exp
    | Cons
    | Num of int
    | Make of Exp * Exp
    | Return

type Value =
    | NUM of int
    | CONS of Value * Value

(*Extracts the INT of VALUE*)
let GetVal (v:Exp)  = 
    match v with 
    | Num n -> n
    | _ -> 69000



let rec evalExp(exp : Exp, stack : Value list) : Exp =
    match exp with
    | Num n -> Num n
    | Cons -> exp
    | Case (pred, post, after) ->
        let cond, tail = 
            match stack with
            | head::tail -> head, tail
            | _ -> NUM 45, []

        let predT = evalExp(pred, stack)

        match cond, predT with
        | (CONS (fst, snd)), Cons -> 
            printfn "Both cons"
            evalExp(post, fst::snd::tail)
            
        | NUM nv, Num ne -> 
            printf "Both num"
            if (nv = ne) then
                printfn " Equal"
                evalExp(post, tail)
            else
                printfn " Unsame condI = %i | predI = %i" nv ne
                evalExp(after, stack)
        |  _ -> 
            printfn " else"
            evalExp(after, stack)
    | Make (valM, after) ->
        match evalExp(valM, stack) with
        | Cons -> 
            match stack with
            | head1::head2::tail -> 
                evalExp(after, CONS(head1, head2)::tail) 
            | _ -> 
                printfn "Probably an error here1"
                exp
        | Num n -> 
            match stack with
            | head1::tail -> 
                evalExp(after, (NUM n)::tail) 
            | _ -> 
                printfn "Probably an error here2"
                exp
        | _ -> 
            printfn "Probably an error here3"
            exp
    | Return -> 
    | _ -> 
        printfn "Probably an error here4"
        exp
