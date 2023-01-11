module LocalInverter

open AbSyn

/// <summary>
/// Performs local inversion on a grammar program
/// </summary>
let LocalInversion(grammar : GrammarProgram) : GrammarProgram =
    // Find (Make's in grammar and record their arity)
    let mutable userDefinedTypes : (string * int) list = []

    List.map (fun (nt, prodLst) -> 
        (nt, (List.map (fun prod -> 
            List.map (fun term -> 
                match term with
                | Make (s1, num1) ->
                    let existingType =
                        List.tryFind (fun (s2, num2) -> 
                            s1 = s2
                            ) userDefinedTypes
                    match existingType with
                    | Some (s2, num2) -> 
                        if (num1 = num2) then
                            ()
                        else //Duplication and Equality ruins this
                            failwithf "Error: local inversion cannot be done on variable with multiple arities %A" term
                    | None -> userDefinedTypes <- (s1,num1)::userDefinedTypes
                | _ -> ()
                ) prod
            ) prodLst)) 
        ) grammar |> ignore

    //printfn "UDT %A" userDefinedTypes


    let LocalInvertProduction(prod : Production) =
        List.fold (fun (acc  : Production) terminal -> 
            match terminal with
            | Case (s1) ->
                let existingType =
                    List.tryFind (fun (s2, num2) -> 
                        s1 = s2
                        ) userDefinedTypes
                match existingType with
                | None -> failwithf "Error cannot do local inversion on Case of variable, without a known arity %A. Needs to Make such a variable." terminal
                | Some (s2, num2) ->
                    match (List.tryHead acc) with
                    | Some CaseEquality | Some CaseNonEquality -> //The arity doesn't match after local inversion
                        (Make (s2, num2 + 1))::acc
                    | Some Duplicate -> //The arity doesn't match after local inversion
                        (Make (s2, num2 - 1))::acc
                    | _ -> 
                        (Make (s2, num2))::acc
            | Make (s1, num) -> 
                (Case (s1))::acc
            | MakeNum num -> (CaseNum num)::acc
            | CaseNum num -> (MakeNum num)::acc
            | Nonterminal (s1) -> terminal::acc
            | Permute intLst -> 
                let mutable newOrder = Array.zeroCreate (intLst.Length)
                let mutable counter = 1
                for elem in intLst do
                    newOrder.[elem - 1] <- counter
                    counter <- counter + 1
                (Permute (List.ofArray newOrder))::acc
            | Duplicate -> 
                CaseEquality::acc
            | CaseEquality -> 
                Duplicate::acc
            | CaseNonEquality ->
                CaseNonEquality::acc
            ) [] prod

    List.map (fun (nt, prodLst) -> 
        (nt, (List.map (fun prod -> 
            LocalInvertProduction(prod)
            ) prodLst)) 
        ) grammar