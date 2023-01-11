module Interpreter

open AbSyn

/// <summary>
/// Pretty prints given stack to a string.
/// </summary>
let PPStack(stack) : string =
    let mutable retString = "["

    let rec printStackElem(elem) =
        match elem with
        | CONS (s1, lst) -> 
            if (lst = []) then
                retString <- retString + (sprintf "%s" s1)
            else
                retString <- retString + (sprintf "%s (" s1)
                PrintList(lst, ", ")
                retString <- retString + (sprintf ")")
        | NUM i ->
            retString <- retString + (sprintf "%A" i)

    and PrintList(lst, str) =
        match lst with
        | [last] -> 
            printStackElem(last)    
        | head::tail ->    
            printStackElem(head)
            retString <- retString + (sprintf "%s" str)
            PrintList(tail, str)
        | _ -> ()

    PrintList(stack, "; ")
    retString <- retString + "]\n"
    retString

//Describes what happens to the stack after a succeding Case terminal
type stackModifer =
    | NewCons of Value list
    | RemoveElem
    | Add of Value

/// <summary> Test whether case-type terminal corresponds to head of stack </summary>
/// <param name="terminal"> A terminal that should be a case-type </param>
/// <param name="stackHead"> The value which the terminal is to affect </param>
/// <returns> A stackModifer option signaling what should happen to the stack, along with succeding terminal </returns>
let TryCase(terminal: GrammarTerminal, stackHead : Value) : stackModifer option =
    match terminal with
    | Case s1 -> 
        match stackHead with
        | CONS (s2, paramLst) -> 
            if (s1 = s2) then Some (NewCons paramLst)
            else None
        | _ -> None
    | CaseNum x -> 
        match stackHead with
        | NUM y -> 
            if (x = y) then Some RemoveElem
            else None
        | _ -> None
    | CaseEquality -> 
        match stackHead with
        | CONS (s1, paramLst) -> 
            match paramLst with
            | elem1::elem2::[] when elem1 = elem2 -> Some (Add (CONS (s1, [elem1])))
            | _ -> None
        | _ -> None
    | CaseNonEquality -> 
        match stackHead with
        | CONS (s1, paramLst) -> 
            match paramLst with
            | elem1::elem2::[] when not (elem1 = elem2) -> Some (Add (CONS (s1, paramLst)))
            | _ -> None
        | _ -> None
    | _ -> 
        failwithf "Tried terminal was not a Case type. Recived %A" terminal


//Combination of List.find and List.tryPick
//Made by user Abel from https://stackoverflow.com/questions/73924712/f-generic-type-constricted-to-specific-type/73925815#73925815
let tryFindPick(list : 'T list, func : 'T -> 'U option) : ('T * 'U) option =
    List.tryPick(fun x -> 
        match func x with
        | Some y -> Some(x, y)
        | None -> None
        ) list


//Given available prods, returns next prods
let NextProds(availableProds : Production list) =
    //Remove first element
    let NewProds = List.map (fun (prod: Production) -> prod.Tail) availableProds
    //Remove empty productions
    List.filter (fun (prod: Production) -> not (prod = [])) NewProds


let RunGrammar(grammar : GrammarProgram, startStack : Value list) =
    
    //To run nonterminals, the program needs to have access to the entire grammar
    let rec RunFromSym(symbol : string, stack : Value list) =
        let ntProdPair = List.tryFind (fun (nonTerm : string, prods : Production list) -> nonTerm = symbol) grammar
        match ntProdPair with
        | None -> 
            failwithf "Error Interpreting: A nonterminal was called which is not in gramamr, Nonterminal was '%A'" symbol
        | Some ntProds -> 
            let initialProduction = snd (ntProds)
        
            evalGrammar(initialProduction, stack)

    and evalGrammar(prods : Production List, stack : Value list) =
        if (prods = [[]]) then
            stack
        else
            //Finds distinct terminals of heads of productions
            let distinctTerminals = List.distinct (List.map (fun (prod : Production) -> prod.Head) prods)
    
            //Tests if productions has cases in all heads of production
            let OnlyCases =
                List.forall (fun terminal -> 
                    match terminal with
                    | Case _ -> true
                    | CaseNum _ -> true
                    | CaseEquality -> true
                    | CaseNonEquality -> true
                    | _ -> false
                ) distinctTerminals

            //Runs next terminal
            match (OnlyCases, distinctTerminals.Length) with
            | true, _ -> //Determinism through cases
                let TopStack : Value option = List.tryHead stack

                if (TopStack = None) then
                    failwithf "Error: Case operation on empty stack \nprods: %A\nStack %A)" prods stack
                match TopStack with
                | None -> failwithf "Error: Case operation on empty stack \nprods: %A\nStack %A)" prods stack
                | Some topStackV -> 
                    let termMod = tryFindPick
                                        (distinctTerminals, 
                                        (fun terminal -> TryCase(terminal, topStackV)))

                    match termMod with 
                    | Some (acceptedTerminal, stackMod)->
                        let mutable newStack = []
                        match stackMod with
                        | NewCons (paramLst) -> 
                            newStack <- paramLst@(stack.Tail)
                        | RemoveElem -> 
                            newStack <- stack.Tail
                        | Add (stackElem) -> 
                            newStack <- stackElem::(stack.Tail)

                        //Productions, following accepted terminal
                        let ApplyableProds = List.filter (fun (prod: Production) -> prod.Head = acceptedTerminal) prods
                        let nextProds = NextProds(ApplyableProds)
                        if (nextProds.IsEmpty) then
                            newStack
                        else
                            evalGrammar(nextProds, newStack)
                    | _ ->
                        failwithf "Error interpreting: Top of stack (%A) did not match any terminals in list: \n%A" topStackV distinctTerminals

            | false, 1 -> //Determinism through options (Only one distinct Terminal)
                let newStack = RunTerminal(distinctTerminals.Head, stack)
                let nextProds = NextProds(prods)
                if (nextProds.IsEmpty) then
                    newStack
                else
                    evalGrammar(nextProds, newStack)

            | _, _ -> //Non deterministic case
                failwithf "Error interpreting: Nondeterministic choice in production list\n%A" prods

    and RunTerminal(terminal: GrammarTerminal, stack : Value list) : Value list =
        match terminal with
        | Make (s1, num) ->
            try 
                let headLst, tail = List.splitAt num stack
                CONS(s1, headLst)::tail
            with
                | Failure msg -> failwithf "Error Interpreting: %A on stack with %A elements" terminal (stack.Length)
        | MakeNum x -> 
            (NUM x)::stack
        | Nonterminal name -> 
            RunFromSym(name, stack)
        | Permute order -> 
            let checkOrder = List.sort order
            let orderMatch = List.init (order.Length) (fun i -> i + 1)

            if (not (checkOrder = orderMatch)) then
                failwithf "Error Interpreting: Illegal permutation %A" terminal

            if (order.Length > stack.Length) then
                failwithf "Error Interpreting: Permutating out of stack. Perumtation: %A\nStack: %A" terminal (PPStack(stack))

            let headLst, tail = List.splitAt (order.Length) stack
            let newHead = List.map (fun index -> headLst.Item(index - 1)) order
            
            newHead @ tail
        | Duplicate ->
            let TopStack : Value option = List.tryHead stack

            match TopStack with 
            | Some (CONS (s1, lst)) ->
                match lst with
                | [elem] -> (CONS (s1, (lst @ lst)))::(stack.Tail)
                | _ -> failwithf "Error Interpreting: Duplication operation on non-unary item \nStack %A)" stack
            | None -> failwithf "Error Interpreting: Duplication operation on empty stack \nStack %A)" stack
            | Some x -> failwithf "Error Interpreting: Duplication on non Cons element, StackTop = %A)" x
        //Add other operations here
        | _ -> failwithf "Error Interpreting: Unexpected terminal '%A', when running" terminal
    
    let startSymbol = 
        match (List.tryHead grammar) with
        | Some (ss, sp) -> ss
        | _ -> failwithf "Error Interpreting: Failed to find start of grammar :/\n%A" grammar
    RunFromSym(startSymbol, startStack)



let PrettyPrintStack(grammar : GrammarProgram, stack : Value list) = 
    let resStack = RunGrammar(grammar, stack)
    PPStack(resStack)