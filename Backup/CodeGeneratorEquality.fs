module CodeGenerator

open AbSyn

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



let rec ResolveFctTerm(term : fctSyntaxTerminal, varTracker : VariableTracker) : Production =
    match term with
    | FMake (s1, argNum) -> 
        varTracker.Drop(argNum)
        varTracker.AddUnnamed()
        [Make (s1, argNum)]
    | FCaseVar (s1, sLst)  -> 
        varTracker.AddVar(sLst)
        
        [Case s1]

    | FCase varFct -> 
        let res = MakePermutation(varFct, varTracker)
        varTracker.Drop(1)
        res
    | FCaseNum num -> [CaseNum num]
    | FMakeNum num -> 
        varTracker.AddUnnamed()
        [MakeNum num]
    | FUseVar varFct -> MakePermutation(varFct, varTracker)
    | FNonterminal (nTerm, parameters) -> 
        let ret = (List.foldBack 
            (fun param acc -> 
                List.fold (fun acc2 x -> acc2 @ ResolveFctTerm(x, varTracker)) acc param
            ) parameters []
        @ [Nonterminal nTerm])
        
        for elem in parameters do
            varTracker.Drop(1)

        ret
    | FAddVar lst -> 
        varTracker.AddVar(lst)
        []
    | FDuplicate -> 
        [Duplicate]
    | FEquality -> 
        [Equality]
    | FRemoveTop ->
        varTracker.Drop(1)
        []


let GenerateProduction(fctProduction : fctProduction, varTracker : VariableTracker)  =
    List.fold 
        (fun acc (term : fctSyntaxTerminal) -> 
            try acc @ ResolveFctTerm(term, varTracker)
            with Failure msg -> failwithf "%A\n%A" msg fctProduction
        ) [] fctProduction


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
