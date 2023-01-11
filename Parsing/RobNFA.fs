module RobNFA

open AbSyn


/// <summary> 
/// NFA state object
/// </summary> 
type state(reduce : (int * string) option) =
    let mutable transitionList : (state * NFATerminal) list = []

    member this.transitionLst
        with get() = transitionList
        and set(transLst) = transitionList <- transLst

    member this.reducing = reduce

    override this.ToString() = "NFA-state"


/// <summary> Creates an NFA from a grammar program, where each state has a value signifing their distance from start of production - 1 </summary>
/// <param name="grammar"> Grammar program to generate NFA of </param>
/// <returns> NFA, with each state having a field for distance from start of production</returns>
let CreateNFARS(grammar : GrammarProgram) : state option =
    //Create starting state for each nonterminal (Epislon transitions point to these)
    let nonterminalTable : (string * state) list = 
        List.map 
            (fun (nonTerm, prodLst) -> (nonTerm, state(None))
            ) grammar

    //For each nonterminal
    for (nonTerm, prodLst) in grammar do
        //Creates path from Nonterminal to all its productions
        for production in prodLst do
            
            let mutable distance = 0;
            
            let mutable currentState =  
                match (List.tryFind (fun (nt, state) -> nt = nonTerm) nonterminalTable) with
                | Some (nt, state) -> state
                | _ -> failwithf "Error in function CreateNFA"
           
            //Creates path along one production
            for terminal in production do
                let mutable nextState = state(Some (distance, nonTerm))

                match terminal with 
                | Nonterminal name ->
                    let nonterminalState =  
                        match (List.tryFind (fun (nt, state) -> nt = name) nonterminalTable) with
                        | Some (nt, state) -> 
                            //printfn "tried to find %A" name
                            state
                        | _ -> failwithf "Error in CreateNFA, Could not find nonterminal %A" name

                    currentState.transitionLst <- 
                        [nextState, Terminal terminal; nonterminalState, Epsilon name] 
                        @ currentState.transitionLst
                | _ ->
                    currentState.transitionLst <- (nextState, Terminal terminal) :: currentState.transitionLst
                currentState <- nextState

                distance <- distance + 1

    match nonterminalTable with
    | (nonterm, initialState)::tail ->
        Some initialState
    | _ -> None

/// <summary> 
/// Prints an NFA
/// </summary> 
/// <param name ="nfastate"> starting state of NFA </param>
/// <param name ="depth"> Initail depth, please set to 0 </param>
/// <param name ="nfastate"> Max depth to search through NFA states </param>
let rec PrintNFA(nfaState : state, depth : int, max : int) =
    if (depth < max) then
        for (nextState, terminal) in nfaState.transitionLst do
            for i = 1 to (depth) do printf "  "
            printfn "%A" terminal
            PrintNFA(nextState, depth + 1, max)
