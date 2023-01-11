module CodeGenerator

open AbSyn


/// <summary>
/// Class for tracking used variables in FCT Grammar
/// </summary>
type VariableTracker( varList : string list) =
    let mutable NamedValues = List.map (fun x -> Some x) varList

    member this.Lst
        with get() = NamedValues

    member this.Top = NamedValues.Head //Unused?

    member this.AddVar( addedVar : string list ) =
        let adding = List.map (fun x -> Some x) addedVar
        NamedValues <- adding @ NamedValues

    member this.AddUnnamed() = NamedValues <- None::NamedValues

    member this.NameTop(addedVar : string) =
        NamedValues <- (Some addedVar)::NamedValues.Tail

    member this.ReplaceVar( addedVar : string list ) = //Unused?
        let adding = List.map (fun x -> Some x) addedVar
        NamedValues <- adding @ NamedValues.Tail

    member this.Drop(x : int) =
        NamedValues <- NamedValues.[x..]

    member this.RemoveElem(varIndex : int) =
        NamedValues <- List.removeAt (varIndex) NamedValues
        
    override this.ToString() = (sprintf "Tracking: %A" NamedValues)


/// <summary>
/// Calculates appropriate permutation given a requested first variable, 
/// by finding the variable in the variable tracker.
/// </summary>
let MakePermutation(varFct : string, varTracker : VariableTracker) =
    let ReturnError(msg : string) =
        failwithf "Error, %A, in:\n%A" msg (varTracker.ToString())

    let itemNum = List.tryFindIndex (fun x -> x = Some varFct) varTracker.Lst
    match itemNum with
    | Some index -> 
        if (index = 0) then
            []
        else
            let unPermuted = 
                if (index >= 1) then
                    List.init (index) (fun v -> v + 1)
                else []
            let permutation = (index + 1) :: unPermuted
            
            varTracker.RemoveElem(index)
            varTracker.AddVar([varFct])
            [Permute permutation]
    | None -> 
        ReturnError("'" + varFct + "' is used before generated")


/// <summary>
/// Translates FCT production to Grammar production
/// </summary>
let GenerateProduction(fctProduction : fctProduction, varTracker : VariableTracker)  =
    let mutable previousWasEquality = false

    let rec ResolveFctTerm(term : fctSyntaxTerminal, varTrackerInner : VariableTracker) : Production =
        //printfn "\nterm: %A\n%A" term (varTrackerInner.ToString())

        match (previousWasEquality, term) with
        | (true, FCaseVar (s1, sLst)) ->
            previousWasEquality <- false
            varTrackerInner.AddVar(sLst)
            match sLst with
            | s2::s3::[] -> 
                // As Neq is often (Make tuple 2, NEQ, ...)
                // We need a case after NEQ such that local inversion creates
                // A make on the other side of the NEQ.
                // This cannot be done with having NEQ strip the variable regardless of its value 
                // As that would require storing the variable. 
                // tl;dr backwards NEQ should not make tuple 2 for the case above
                [CaseNonEquality; Case s1]
            | s2::[] ->
                [CaseEquality]
            | _ -> failwithf "Error Code-generation: Illegal case after equality"
        | _ ->
            previousWasEquality <- false
            match term with
            | FMake (s1, argNum) -> 
                varTrackerInner.Drop(argNum)
                varTrackerInner.AddUnnamed()
                [Make (s1, argNum)]
            | FCaseVar (s1, sLst)  -> 
                varTrackerInner.AddVar(sLst)
        
                [Case s1]

            | FCase varFct -> 
                let res = MakePermutation(varFct, varTrackerInner)
                varTrackerInner.Drop(1)
                res
            | FCaseNum num -> [CaseNum num]
            | FMakeNum num -> 
                varTrackerInner.AddUnnamed()
                [MakeNum num]
            | FUseVar varFct -> MakePermutation(varFct, varTrackerInner)
            | FNonterminal (nTerm, parameters) -> 
                let ret = (List.foldBack 
                    (fun param acc -> 
                        List.fold (fun acc2 x -> acc2 @ ResolveFctTerm(x, varTrackerInner)) acc param
                    ) parameters []
                @ [Nonterminal nTerm])
        
                for elem in parameters do
                    varTrackerInner.Drop(1)

                ret
            | FAddVar lst -> 
                varTrackerInner.AddVar(lst)
                []
            | FDuplicate -> 
                varTrackerInner.Drop(1)
                [Duplicate]
            | FEquality -> 
                previousWasEquality <- true
                varTrackerInner.Drop(1)
                []
            | FRemoveTop ->
                varTrackerInner.Drop(1)
                []


    List.fold 
        (fun acc (term : fctSyntaxTerminal) -> 
            try 
                acc @ ResolveFctTerm(term, varTracker)
            with Failure msg -> failwithf "%A\n%A" msg fctProduction
        ) [] fctProduction

/// <summary>
/// Merges consecutive permutations
/// </summary>
let OptimizePermutation(grammar : GrammarProgram) : GrammarProgram =
    let permutePermutation(more, less) =
        let mutable counter = -1
        List.map (fun elem -> 
            match (List.contains (elem) less) with
            | true -> 
                counter <- counter + 1
                List.item (counter) less
            | false -> elem
            ) more

    let FixPermInProduction(prod : Production) =
        let fixedProd =
            List.fold (fun (acc  : Production) terminal -> 
                    match terminal with
                    | Permute order -> 
                        match (List.tryHead acc) with
                        | Some (Permute order2) -> 
                            let newOrder =
                                if (order2.Length >= order.Length) then
                                    let tail = order2.[order.Length..]
                                    let head =
                                        List.map (fun i -> 
                                                order2.[i - 1]
                                            ) order
                                    head@tail
                                else
                                    permutePermutation(order, order2)

                            let SequentialPermutation = List.init (newOrder.Length) (fun v -> v + 1)
                            match (List.forall2 (=) newOrder SequentialPermutation) with
                            | true -> (acc.Tail)
                            | _ -> (Permute newOrder)::(acc.Tail)
                        | _ -> terminal::acc
                    | _ -> terminal::acc
                ) [] prod
        List.rev fixedProd

    List.map (fun (nt, prodLst) -> 
        (nt, (List.map (fun prod -> 
            FixPermInProduction(prod)
            ) prodLst))
        ) grammar

/// <summary>
/// Generates Grammar Program from FCT Grammar
/// </summary>
let GenerateGrammar(fctProg : fctSyntaxGrammar) : GrammarProgram  =
    OptimizePermutation(
        List.map 
            (fun (nontermProds : fctNonterminalProds) -> 
                let fctNt, (NtProds : fctProduction list) 
                    = nontermProds
                let ntName, ntParams = fctNt
            
                let GrammarProdList =
                    List.map 
                        (fun fctProd -> 
                            let tracker = VariableTracker(ntParams)
                            GenerateProduction(fctProd, tracker)
                        ) NtProds

                (ntName, GrammarProdList)
            ) fctProg
    )
