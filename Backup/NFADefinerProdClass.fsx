
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


type state () =
    let mutable transitionList : (state * NFATerminal) list = []

    member this.transitionLst
        with get() = transitionList
        and set(transLst) = transitionList <- transLst

    override this.ToString() = "state"


let inc1 : GrammarProgram = 
    ["inc", 
        [
            [CaseCons; CaseNum 0; CaseNum 1; MakeNum 1]; 
            [CaseCons; CaseNum 1; MakeNum 0; MakeCons]; 
            [CaseCons; CaseNum 0; Nonterminal "inc"; MakeNum 1; MakeCons]
        ]
    ]



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
    let nonterminalTable : (string * state) list = 
        List.rev (List.map 
            (fun (nonTerm, prodLst) -> (nonTerm, state())
            ) grammar)

    //For each nonterminal
    for (nonTerm, prodLst) in grammar do
            //Creates path from Nonterminal to all its productions
            for production in prodLst do
                //Creates path along one production
                let mutable currentState = SearchStateTable(nonterminalTable, nonTerm).Value //Exception if none is returned
                for terminal in production do
                    let nextState = state()
                    currentState.transitionLst <- [nextState, terminal] @ currentState.transitionLst
                    currentState <- nextState

    match nonterminalTable with
    | (nonterm, initialState)::tail ->
        Some initialState
    | _ -> None













    //Subset DFA Grammar
    //Does not work with ANY NFA, only those created from a grammar using createNFA



type productionClass(terminal : NFATerminal, s : state) =
    let mutable transitions : state list = [s]

    member this.transStates = transitions
    member this.termial : NFATerminal = terminal
    member this.UniteStates(state) = 
        transitions <- [state] @ transitions
    //override this.ToString() = "prodClass of " + this.termial.ToString()
    override this.ToString() = this.termial.ToString()


//Searches "productionClass list" to see if a production for given terminal allready exists.
let rec searchForTransition(lst : productionClass list, terminal : NFATerminal) : productionClass option =
    match lst with
    | head::tail ->
        if (terminal = head.termial) then
            Some head
        else
            searchForTransition(tail, terminal)
    | [] -> 
        None



let stateNFA = CreateNFA(inc1).Value





let DistinctTransitions(stateLst : state list) : productionClass list =
    let mutable curNonterm : productionClass list = []
    for state in stateLst do
        for (s : state, terminal : NFATerminal) in state.transitionLst do
            match searchForTransition(curNonterm, terminal) with
            | Some prodClass -> 
                prodClass.UniteStates(s)
            | None ->
                curNonterm <- productionClass(terminal, s)::curNonterm
    curNonterm



let stateTalble = ("inc", stateNFA)

let mutable counter = 0


let mutable currentTransitions : productionClass list = []

let rec followTest(stateLst : state list) : NFATerminal list =
    currentTransitions <- DistinctTransitions(stateLst)
    match currentTransitions with
    | head::[] ->
        head.termial::followTest(head.transStates)
    | head::tail ->
        let curTerm =("f" + counter.ToString()) 
        [Nonterminal curTerm]
    | _ ->
        []


let rec SubsetGrammarConstruction(stateLst : state list) : GrammarProgram =
    currentTransitions <- DistinctTransitions(stateLst)
    printfn "t %A" currentTransitions

    let mutable grammarProgram : GrammarProgram = 
        ["inc",
            List.map 
                (fun (production : productionClass) -> production.termial::followTest(production.transStates)
                ) currentTransitions
        ]

    while not (currentTransitions = []) do
        let curTerm =("f" + counter.ToString()) 
        counter <- counter + 1 
        grammarProgram <- 
            (curTerm,
                List.map 
                    (fun (production : productionClass) -> 
                        match production.termial with
                        | Epsilon x -> 
                            printfn "DANGER: E %A" x
                            production.termial::followTest(production.transStates)
                        | Nonterminal x -> 
                            printfn "DANGER: N %A" x
                            production.termial::followTest(production.transStates)
                        | _ -> 
                            production.termial::followTest(production.transStates)
                    ) currentTransitions
            ):: grammarProgram 

    List.rev grammarProgram

let res = SubsetGrammarConstruction([stateNFA])
printfn "res \n%A" res





let test : GrammarProgram =
    ["inc", 
        [
            CaseCons::[]; 
            [CaseNum 0]
        ]
    ]
printfn "test \n%A" test

printfn "inc1 \n%A" inc1

(*
let inc1 : GrammarProgram = 
    ["inc", 
        [
            [CaseCons; CaseNum 0; CaseNum 1; MakeNum 1]; 
            [CaseCons; CaseNum 1; MakeNum 0; MakeCons]; 
            [CaseCons; CaseNum 0; Nonterminal "inc"; MakeNum 1; MakeCons]
        ]
    ]
*)