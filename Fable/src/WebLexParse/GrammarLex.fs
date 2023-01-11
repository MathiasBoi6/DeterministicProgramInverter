module GrammarLex

open AbSyn

type WebsiteLine =
    | NonterminalLine
    | ProductionLine
    | EmptyLine


let CleanProduction(prodLine : string) : string [] =
    let cleanProdLine = (((prodLine.Split[|'['|]).[1]).Split[|']'|]).[0]
    cleanProdLine.Trim().Split([|';'|], System.StringSplitOptions.RemoveEmptyEntries)

let StringToGramTerminal(terminal : string) : GrammarTerminal =
    let term2 = (((terminal.Replace("(", " ")).Replace(")", " ")).Replace(",", " ")).Replace("\"", " ")
    let term = term2.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
    match term with
    | [| "Case"; stringType |] -> Case stringType
    | [| "Make"; stringType; stringNum |] -> Make (stringType, int stringNum)
    | [| "MakeNum"; x |] -> MakeNum (int x)
    | [| "CaseNum"; x |] -> CaseNum (int x)
    | [| "Nonterminal"; x |] -> Nonterminal ("\"" + x + "\"")
    | [| "Duplicate" |] -> Duplicate
    | [|"CaseEquality"|] -> CaseEquality
    | [| "CaseNonEquality" |] -> CaseNonEquality
    | _ -> 
        match (term.[0]) with
        | "Permute" ->
            let permuteOrder = 
                Array.fold 
                    (fun acc strig -> 
                        ((int) strig)::acc
                    ) [] term.[1..term.Length] 
            Permute (List.rev permuteOrder)
        | _ -> failwithf "Error, incorrect terminal: %A" term

let ProductionLexer(prodLine : string) : Production =
    let prodArr = CleanProduction(prodLine)
    let prod = List.map (fun term -> StringToGramTerminal(term)) (Array.toList prodArr)
    prod

let CleanNonterminal(ntLine : string) =
    let nt = (ntLine.Split[|' '|]).[0]
    match (nt.[nt.Length - 1]) with
    | ':' -> nt.[0..nt.Length - 2]
    | _ -> nt

let StringCheckEmpty (grammarString : string) : bool =
    let NumBetween(min, max, c) =
        (min <= int c && int c >= max) 
    Array.forall (fun c -> not (NumBetween(48, 59, c) || NumBetween(65, 93, c) || NumBetween(97, 122, c) )) (grammarString.ToCharArray())
    

let IdentifyGrammar(grammarString : string) : WebsiteLine =
    if (StringCheckEmpty(grammarString)) then
        printfn "%A WAS EMPTY" grammarString
        EmptyLine
    else
        let prodReq1 = grammarString.Contains("[")
        let prodReq2 = grammarString.Contains("]")
        let prodReq3 = grammarString.Contains(";")

        match (prodReq1, prodReq2, prodReq3) with
        | (true, true, _) -> ProductionLine
        | (false, false, false) -> NonterminalLine
        | _ -> EmptyLine

let GrammarLexer(grammarString : string) : GrammarProgram =  
    let grammarStrings = grammarString.Split[|'\n'|]

    let mutable linNum = 1;
    let mutable grammarProgram = [];
    let mutable nonterminal = "";
    let mutable productions = [];

    for line in grammarStrings do
        try 
            match IdentifyGrammar(line) with
            | ProductionLine -> 
                productions <- ProductionLexer(line)::productions
            | NonterminalLine -> 
                if (not (nonterminal = "")) then
                    grammarProgram <- (nonterminal, productions)::grammarProgram
                nonterminal <- CleanNonterminal(line)
                productions <- []
            | _ -> ()
            linNum <- linNum + 1
        with Failure msg -> failwithf "Parse error, line %i\n%A" linNum msg
    grammarProgram <- (nonterminal, productions)::grammarProgram
    grammarProgram <- List.rev grammarProgram
    grammarProgram

let InvertGrammar(grammarProgram : GrammarProgram) =
    let locInvGrammar = LocalInverter.LocalInversion(grammarProgram)

    let nfa = RobNFA.CreateNFARS(locInvGrammar).Value
    let dfa = RobDFA.NFAToDFARS(nfa)

    let collectionOfPPs = RobParse.CalculateProgramPointsInit(dfa)
    let (fs, gs, hs) = RobParse.GenerateGrammarComplete(collectionOfPPs)
    let uncompressedGrammar = (fs, gs)
    let invGrammar = RobParse.TransitionCompression(uncompressedGrammar)
    invGrammar, (fs, gs, hs)