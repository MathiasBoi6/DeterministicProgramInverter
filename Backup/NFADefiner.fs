module GrammarToNFA

type NFATerminal =
    | CaseCons
    | MakeCons
    | MakeNum of int
    | CaseNum of int
    | Nonterminal of string
    | Epsilon of string //If one state has two productions, we can see which reduce with which nonterminal

type Production = NFATerminal list

type Nonterminal = string * Production list

type GrammarProgram = Nonterminal list


type state(accepting : (string * int) option) =
    let mutable transitionList : (state * NFATerminal) list = []

    member this.transitionLst
        with get() = transitionList
        and set(transLst) = transitionList <- transLst

    member this.accepting = accepting

    override this.ToString() = "NFA-state"


//Searches a tuple list with signature "(string * state) list"
//And returns "state" when key = string
let rec SearchStateTable(lst : (string * state) list, key : string) : state option =
    match lst with
    | (str, elem)::tail ->
        if (key = str) then
            Some elem
        else
            SearchStateTable(tail, key)
    | [] -> 
        None


//The NFA created does not have any states marked as accepting.
//As the last state of any branch should always be accepting, we know that any state with no transitions, must be accepting.
////Could optimize, by removing nonterminal transitions that lead to nothing. ?????? what did i mean by this, perhaps reduce instead
let CreateNFA(grammar : GrammarProgram) : state option =
    //Table of nfa states, starts with one state per nonterminal
    let nonterminalTable : (string * state) list = 
        List.rev (List.map 
            (fun (nonTerm, prodLst) -> (nonTerm, state(None))
            ) grammar)

    //For each nonterminal
    for (nonTerm, prodLst) in grammar do
            //Creates path from Nonterminal to all its productions
            for production in prodLst do
                //Creates path along each production
                let mutable currentState = SearchStateTable(nonterminalTable, nonTerm).Value //Exception if none is returned
                List.mapi (fun i terminal -> 
                    let mutable nextState = state(None)
                    if (i = production.Length - 1) then         
                        nextState <- state(Some (nonTerm, production.Length))
                    currentState.transitionLst <- [nextState, terminal] @ currentState.transitionLst
                    currentState <- nextState
                    ) production |> ignore

    match nonterminalTable with
    | (nonterm, initialState)::tail ->
        Some initialState
    | _ -> None


let CreateNFAwitEps(grammar : GrammarProgram) : state option =
    let nonterminalTable : (string * state) list = 
        List.rev (List.map 
            (fun (nonTerm, prodLst) -> (nonTerm, state(None))
            ) grammar)

    //For each nonterminal
    for (nonTerm, prodLst) in grammar do
            //Creates path from Nonterminal to all its productions
            for production in prodLst do
                //Creates path along one production
                let mutable currentState = SearchStateTable(nonterminalTable, nonTerm).Value //Exception if none is returned
                let mutable iteratorI = 0
                for terminal in production do
                    let mutable nextState = state(None)
                    if (iteratorI = production.Length - 1) then
                        nextState <- state(Some (nonTerm, production.Length))
                    match terminal with 
                    | Nonterminal name ->
                        currentState.transitionLst <- 
                            [nextState, terminal; 
                             SearchStateTable(nonterminalTable, name).Value, Epsilon name] 
                            @ currentState.transitionLst
                    | _ ->
                        currentState.transitionLst <- [nextState, terminal] @ currentState.transitionLst
                    currentState <- nextState

                    iteratorI <- iteratorI + 1

    match nonterminalTable with
    | (nonterm, initialState)::tail ->
        Some initialState
    | _ -> None