
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
            [CaseNum 0; CaseNum 1; MakeNum 0; MakeCons]; 
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
                    match terminal with 
                    | Nonterminal name ->
                        let nextState = state()
                        currentState.transitionLst <- 
                            [nextState, terminal; 
                             SearchStateTable(nonterminalTable, name).Value, Epsilon name] 
                            @ currentState.transitionLst
                        currentState <- nextState
                    | _ ->
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
    override this.ToString() = "prodClass of " + this.termial.ToString()


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



(*
let mutable counter = 0

let rec evalState(state : state) =
    match state.transitionLst with
    | (nextState, transTerminal)::[] ->
        [transTerminal] @ evalState(nextState)
    | head::tail ->
        let nontermName = ("f" + counter.ToString())
        counter <- counter + 1
        [Nonterminal nontermName]
    | [] ->
        []

printfn "%A" (evalState(stateNFA))
*)


let rec statListTest(stateLst : state list) : GrammarProgram =
    match stateLst with
    | head::[] -> 

        (*let test =
            ["f1",
                [nonterminal; ]
            ]
        *)

        printfn "No head2? %A" head.transitionLst
        //Follow till end
    | head::tail -> 
        printfn "HEJ"
        //Make nonterminal | fx -> head fx+1
    | [] -> 
        printfn "HEJ"
    inc1


let stateTalble = ("inc", stateNFA)


let rec SubsetGrammarConstruction(stateLst : state list) =

    let mutable nonterms : productionClass list = []
    for state in stateLst do
        for (s : state, terminal : NFATerminal) in state.transitionLst do
            match searchForTransition(nonterms, terminal) with
            | Some prodClass -> 
                prodClass.UniteStates(s)
            | None ->
                nonterms <- productionClass(terminal, s)::nonterms
    
    printf "%A   " nonterms.Length

    for nonterminal in nonterms do
        printfn "%A, %A" nonterminal nonterminal.transStates
        match nonterminal.termial with
        | Epsilon x -> 
            printfn "%A" nonterminal.termial
        | _ ->
            SubsetGrammarConstruction(nonterminal.transStates)

    if nonterms = [] then
        printfn "[]"

        (*

        match nonterminal.transition with
        | head::[] -> 
            (*let test =
                ["f1",
                    [nonterminal; ]
                ]
            *)

            printfn "No head? %A" head.transitionLst
            printfn "%A" nonterminal
            //Follow till end
        | head::tail -> 
            printfn "%A" nonterminal
            //Make nonterminal | fx -> head fx+1
        | [] -> 
            printfn "%A" nonterminal
            //End, accepting
        *)



SubsetGrammarConstruction([stateNFA])


(*


let test : Nonterminal =
    "test", 
    List.mapi (fun i (prodClass : productionClass) -> [prodClass.termial; Nonterminal (i.ToString())]) nonterms
printfn "hje %A" test
*)



(*

type grammarHelp() =
    let mutable transitions : (NFATerminal * state list) list = []
    member this.Add() = printfn "hej"


let rec SearchStateList(lst : (NFATerminal * state list) list, key : NFATerminal) : state list option =
    match lst with
    | (str, elem)::tail ->
        if (key = str) then
            Some elem
        else
            SearchStateList(tail, key)
    | [] -> 
        None


let SubsetGrammarConstruction(state : state) : GrammarProgram =
    
    //let test =
        //List.distinct (List.map (fun (stat, nonterm) -> nonterm)  state.transition)
    let mutable transitions : (NFATerminal * state list) list = []
    for (stat, terminal) in state.transition do
        printfn "Hello World"




    ["inc", 
        [
            [CaseCons; CaseNum 0; CaseNum 1; MakeNum 1]; 
            [CaseCons; CaseNum 1; MakeNum 0; MakeCons]; 
            [CaseCons; CaseNum 0; Nonterminal "inc"; MakeNum 1; MakeCons]
        ]
    ]
    *)