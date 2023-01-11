#r @"NFADefiner.dll"

open GrammarToNFA


let inc1 : GrammarProgram = 
    ["inc", 
        [
            [CaseCons; CaseNum 0; CaseNum 1; MakeNum 1]; 
            [CaseCons; CaseNum 1; MakeNum 0; MakeCons]; 
            [CaseCons; CaseNum 0; Nonterminal "inc"; MakeNum 1; MakeCons]
        ]
    ]

let nfa = CreateNFAwitEps(inc1).Value

type FlooredDFA(state: state) = 
    let mutable stateLst : state list = [state]
    let mutable floors : FlooredDFA list = [] 
    let mutable transitions = []

    member this.States = stateLst
    member this.UniteState(state) = 
        stateLst <- [state] @ stateLst

    member this.Floors = floors
    member this.AddFloor(flooredDFA) = 
        floors <- flooredDFA::floors

    member this.Transitions
        with get() = transitions
        and set(transLst) = transitions <- transLst



type DFATransitionList = (NFATerminal * FlooredDFA) list

type TransitionList() =
    let mutable Transitions : (DFATransitionList) = []
    
    let rec SearchTerminal(nfaTerminal : NFATerminal, dfaTrans : DFATransitionList) : FlooredDFA option =
        match dfaTrans with
        | (terminal, dfaState)::tail ->
            if (terminal = nfaTerminal) then
                Some dfaState
            else SearchTerminal(nfaTerminal, tail)
        | _ -> None

    
    member this.AddTransition( (state, terminal) : state * NFATerminal) : FlooredDFA option =
        match SearchTerminal(terminal, Transitions) with
        | Some dfaState ->
            dfaState.UniteState(state)
            None
        | None -> 
            let newDfa = FlooredDFA(state)
            Transitions <- (terminal, newDfa)::Transitions
            Some newDfa

    member this.transes
        with get() = Transitions

    override this.ToString() = "DFA-trans-list"



    

let CreateFlooredDFA(dfa : FlooredDFA) =
    let mutable dfaTable = []

    //Closure
    let FindTransitions(dfaF : FlooredDFA) =
        let transLst = TransitionList()
        for nfaState in dfaF.States do
            for transition in nfaState.transitionLst do 
                match transLst.AddTransition(transition) with
                | Some dfa -> 
                    dfaTable <- dfa::dfaTable
                | _ -> 
                    printfn "Hej"
                printfn "Hej"
            printfn "Hej"
        printfn "Hej"



    printfn "Hej"