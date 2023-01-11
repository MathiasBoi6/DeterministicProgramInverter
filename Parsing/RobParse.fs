module RobParse

open RobNFA
open RobDFA
open AbSyn


type ProgramPoint = DFAState list


/// <summary>
/// Calculates Take and Drop of a program point
/// </summary>
/// <returns> Returns Take set and Drop set, (Take * Drop) </returns>
let TakeAndDrop(progPoint : ProgramPoint) =
    let head = progPoint.Head
    let tail = progPoint.Tail
    if (head.Reducing.Length = 1 ) || (List.contains (head) tail) then
        ([head], tail)
    else
        (progPoint, [])
    

/// <summary>
/// Calculates collection of program points
/// </summary>
/// <returns> Returns collection of program points of given DFA </returns>
let rec CalculateProgramPoints(progPoint : ProgramPoint, ppList : ProgramPoint list) =
    let take, drop = TakeAndDrop(progPoint) 
    let takenPPlist =
        List.fold 
            (fun acc (term, newDFA) -> 
                match term with
                | Nonterminal _ ->
                    acc
                | _ ->
                    let newPPoint = newDFA::take

                    if not (List.contains (newPPoint) acc) then
                        CalculateProgramPoints(newPPoint, newPPoint::acc)
                    else
                        acc
            ) ppList progPoint.Head.Trans

    //Taken list with dropped list of program points
    let collectionPP =
        List.fold 
            (fun acc (n, h) ->
                if (drop.Length > n) then
                    let reducedPP = drop.[n..]
                    let ReducingTransition = //There can only be one transition on each nonterminal in a DFA state.
                        List.tryFind (fun (term, newDFA) -> term = (Nonterminal h)) reducedPP.Head.Trans

                    match ReducingTransition with
                    | Some (_, newDFA) -> 
                        let newPPoint = newDFA::reducedPP
                        //printfn "newPPoint = %A" newPPoint
                        if not (List.contains (newPPoint) acc) then
                            CalculateProgramPoints(newPPoint, newPPoint::acc)
                        else
                            acc
                    | _ -> acc
                else 
                    acc
            ) takenPPlist progPoint.Head.Reducing
        
    collectionPP

/// <summary>
/// Calculates collection of program points
/// </summary>
let CalculateProgramPointsInit(dfaState : DFAState) =
    let progPoint = [dfaState]
    CalculateProgramPoints(progPoint, [progPoint])    


/// <summary>
/// Prints collection of program points
/// </summary>
let PrintProgramPoints(collectionOfPPs : ProgramPoint list) : unit=
    List.map (fun PP -> printfn "%A" PP) collectionOfPPs |> ignore


//Intermediate Production identifier
type intProdID =
    | F of int list
    | G of int list
    | Eps
    | H of string * int

//Production of type: F[2] -> terminal F[3] G[3:2]
type IntermediateProduction = 
    | Fprod of intProdID * GrammarTerminal * intProdID * intProdID
    | Gprod of intProdID * intProdID * intProdID

/// <summary>
/// Translates program point to a list of integers, representing the points
/// </summary>
let PPtoIntlist(progPoint : ProgramPoint) : int list =
    List.map (fun (dfa : DFAState) -> dfa.Name) progPoint 

/// <summary>
/// Calculates callf and callg, described in "A Method for Automatic Program Inversion Based on LR(0) Parsing".
/// </summary>
/// <returns> (callf, callg) </returns>.
let callfg(progPoint : ProgramPoint) : intProdID * intProdID =
    let take, drop = TakeAndDrop(progPoint) 
    match take, drop with
    | t,[] when t.Head.Trans = [] -> 
        (Eps, Eps)
    | t,d when t.Head.Trans = [] -> 
        (Eps, G (PPtoIntlist(progPoint)))
    | t, [] ->
        (F (PPtoIntlist(take)), Eps)
    | _ ->
        (F (PPtoIntlist(take)), G (PPtoIntlist(progPoint)))

/// <summary>
/// Generates productions that are caused by shifting.
/// </summary>
/// <param name="progPoint"> A taken program point, from fst of TakeAndDrop() </param>
let GenerateShift(progPoint : ProgramPoint) : (GrammarTerminal * (intProdID * intProdID)) list  =
    let transWIthoutNonterm = 
        List.filter 
            (fun (term, newDFA) -> 
                match term with
                | Nonterminal _ -> false
                | _ -> true
            ) progPoint.Head.Trans
    List.map (fun (term, newDFA) -> term, callfg(newDFA::progPoint)) transWIthoutNonterm


/// <summary>
/// Generates productions that are caused by reducing (cont as continues).
/// </summary>
let cont(drop : ProgramPoint, h : string, n : int) : (intProdID * intProdID) option =
    if (drop.Length > n) then
        let reducedPP = drop.[n..]
        let ReducingTransition = //There can only be one transition on each nonterminal in a DFA state.
            List.tryFind (fun (term, newDFA) -> term = (Nonterminal h)) reducedPP.Head.Trans
        match ReducingTransition with
        | Some (_, newDFA) -> 
            let newPPoint = newDFA::reducedPP
            let callf, callg = callfg(newPPoint)
            Some (callf, callg)
        | _ -> 
            if (drop.Head.Reducing.Length = 1) then
                None
            else 
                Some (Eps, H (h, n - drop.Length))
    else
        None

/// <summary>
/// Generates reductions
/// G functions with h's are stored in a string list hFunc
/// </summary>
let GenerateReduce(progPoint : ProgramPoint) : ((IntermediateProduction option) list * string list) option =
    let take, drop = TakeAndDrop(progPoint) 
    match take, drop with
    | _, [] -> 
        None
    | t, _ when t.Head.Reducing = [] -> 
        None
    | t, d -> 
        let mutable hFunc = []
        if (not (t.Head.Reducing.Length = 1)) then
            List.map 
                (fun (n, h) ->
                    let htext = sprintf "%A -> Case %A %A" (G (PPtoIntlist(progPoint))) h n
                    hFunc <- [htext]@hFunc
                ) t.Head.Reducing |> ignore

        let gFunc = (List.map 
            (fun (n, h) -> 
                match cont(d, h, n) with
                | Some (Eps, H (h2, n2)) -> 
                    let htext = sprintf "%A -> Make %A %A" (G (PPtoIntlist(progPoint))) h2 n2
                    hFunc <- [htext]@hFunc
                    None
                | Some (callf, callg) -> 
                    let gprod = G (PPtoIntlist(progPoint))
                    Some (Gprod (gprod, callf, callg))
                | None -> 
                    None
            ) t.Head.Reducing)
        Some (gFunc, hFunc)


/// <summary>
/// Generates a grammar program from a collection of program points
/// Including G functions with H's in them
/// </summary>
let GenerateGrammarComplete(ppList : ProgramPoint list) : (IntermediateProduction list) * (IntermediateProduction list) * (string list) =
    let shifts =
        List.fold 
            (fun acc (progPoint : ProgramPoint) -> 
                let take, drop = TakeAndDrop(progPoint) 
                let genshifts = GenerateShift(take)
                let shiftList =
                    List.map 
                        (fun (term, (callf, callg)) -> 
                            Fprod ((F (PPtoIntlist(take))), term, callf, callg)
                        ) genshifts
                shiftList @ acc
            ) [] ppList
            
    let mutable hList = []
    let reductions =
        let genred = List.map (fun progPoint -> GenerateReduce(progPoint)) ppList
        List.fold 
            (fun acc redList ->
                match redList with
                | Some (red, hlist) -> 
                    hList <- hlist@hList
                    List.fold 
                        (fun acc2 interProdOption -> 
                            match interProdOption with
                            | Some intProd -> 
                                intProd::acc2
                            | None -> 
                                acc2
                        ) acc red
                | _ -> acc
            ) [] genred

    ((List.distinct shifts), (List.distinct reductions), (List.distinct hList))


/// <summary>
/// Generates a grammar program from a collection of program points
/// But excludes productions with h's in them
/// </summary>
let GenerateGrammar(ppList : ProgramPoint list) : (IntermediateProduction list) * (IntermediateProduction list) =
    match GenerateGrammarComplete(ppList) with
    | fs, gs, hs -> (fs, gs)


/// <summary>
/// Prints uncompressed grammar
/// </summary>
let PrintUncompressedGrammar((shifts, reductions): (IntermediateProduction list) * (IntermediateProduction list)) : unit=
    List.map 
        (fun fProd -> 
            match fProd with
            | Fprod (f1 : intProdID, term : GrammarTerminal, f2 : intProdID, g : intProdID) ->
                printfn "%A -> %A %A %A" f1 term f2 g
            | _ -> ()
        ) shifts |> ignore

    List.map 
        (fun gProd -> 
            match gProd with
            | Gprod (g1 : intProdID, f1 : intProdID, g2 : intProdID) ->
                printfn "%A -> %A %A" g1 f1 g2
            | _ -> ()
        ) reductions |> ignore

/// <summary>
/// Finds productions that start with a given nonterminal (key)
/// </summary>
let FilterProds(intProdLst : IntermediateProduction list, key : intProdID) =
    List.filter
        (fun intProd ->
            match intProd with
            | Fprod (prodName, _, _, _) -> key = prodName
            | Gprod (prodName, _, _) -> key = prodName 
        ) intProdLst


/// <summary>
/// Removes redundant productions and compresses grammar program using transitivity
/// </summary>
let TransitionCompression(uncompressedGrammar : (IntermediateProduction list) * (IntermediateProduction list)) =
    let fs, gs = uncompressedGrammar
    let firstNonterminal =
        match fs.Head with
        | Fprod (f1 : intProdID, _, _, _) -> f1
        | _ -> failwithf "A non F function was found in shifts %A" fs.Head
    let mutable NonterminalList : intProdID list = [firstNonterminal]
    let mutable count = 0

    let rec GenerateProd(curProd : IntermediateProduction) : Production =
        match curProd with
        | Fprod (f1 : intProdID, term : GrammarTerminal, f2 : intProdID, g : intProdID) ->
            let nextProdsF = FilterProds(fs, f2)
            let nextProdsG = FilterProds(gs, g)
            let prodF =
                match nextProdsF with
                | [nextProd] -> 
                    term::(GenerateProd(nextProd))
                | [] -> 
                    [term]
                | _ ->
                    if not (List.contains f2 NonterminalList) then
                        NonterminalList <- NonterminalList @ [f2]
                    term::[Nonterminal (f2.ToString())]
            let prodG =
                match nextProdsG with
                | [nextProd] -> 
                    (GenerateProd(nextProd))
                | [] -> 
                    []
                | _ ->
                    if not (List.contains g NonterminalList) then
                        NonterminalList <- NonterminalList @ [g]
                    [Nonterminal (g.ToString())]
            
            prodF@prodG

        | Gprod (g1 : intProdID, f1 : intProdID, g2 : intProdID) ->
            let nextProdsF = FilterProds(fs, f1)
            let nextProdsG = FilterProds(gs, g2)

            let prodF =
                match nextProdsF with
                | [nextProd] -> 
                    (GenerateProd(nextProd))
                | [] -> 
                    []
                | _ ->
                    if not (List.contains f1 NonterminalList) then
                        NonterminalList <- NonterminalList @ [f1]
                    [Nonterminal (f1.ToString())]
            let prodG =
                match nextProdsG with
                | [nextProd] -> 
                    (GenerateProd(nextProd))
                | [] -> 
                    []
                | _ ->
                    if not (List.contains g2 NonterminalList) then
                        NonterminalList <- NonterminalList @ [g2]
                    [Nonterminal (g2.ToString())]

            prodF@prodG

    let mutable grammar : GrammarProgram = []


    while NonterminalList.Length > count do
        let head = NonterminalList.[count]
        count <- count + 1 

        let test = 
            List.map 
                (fun (prod : IntermediateProduction) ->
                    GenerateProd(prod)
                ) (FilterProds(fs, head))
        grammar <- (head.ToString(), test)::grammar

    List.rev grammar

/// <summary>
/// Prints a grammar program
/// </summary>
let PrintGrammar(grammar : GrammarProgram) =
    for (nt, prods) in grammar do
        printfn "%A:" nt
        for x in prods do
            printfn "  %A" x