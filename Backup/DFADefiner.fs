module NFAToDFA

open GrammarToNFA


type DFAState(state: state) =
    let mutable stateLst : state list = [state]
    let mutable reducing = 
        match state.accepting with
        | None -> []
        | Some reduction -> [reduction]

    //Gets set in Closure()
    let mutable TransitionList : (NFATerminal * DFAState) list = []


    let mutable identifer = -1
    member this.Name
        with get() = identifer
        and set(id) = identifer <- id

    member this.states = stateLst
    member this.UniteState(state) = 
        stateLst <- [state] @ stateLst
        match state.accepting with
        | Some reduction -> 
            reducing <- reduction::reducing
        | _ -> ()

    member this.Reducing = reducing

    member this.Trans
        with get() = TransitionList
        and set(transLst) = TransitionList <- transLst

    override this.ToString() = "DFA-state"


//DFAState also uses this type, but cannot use the alias
type DFATransitionList = (NFATerminal * DFAState) list

type DFATransitionObj() =
    let mutable dfaTransitions : DFATransitionList = []

    let rec SearchTerminal(nfaTerminal : NFATerminal, dfaTrans : DFATransitionList) : DFAState option =
        match dfaTrans with
        | (terminal, dfaState)::tail ->
            if (terminal = nfaTerminal) then
                Some dfaState
            else SearchTerminal(nfaTerminal, tail)
        | _ -> None

    //This could match terminal with epsilon and search those pointed to. BUT that would not always be finite
    member this.AddTransition( (state, terminal) : state * NFATerminal) =
        match SearchTerminal(terminal, dfaTransitions) with
        | Some dfaState ->
            dfaState.UniteState(state)
        | None -> 
            dfaTransitions <- (terminal, DFAState(state))::dfaTransitions

    member this.transes
        with get() = dfaTransitions

    override this.ToString() = "DFA-trans-list"




let NFAToDFA(nfaState : state) : DFAState=
    //To have DFA states that point to the same state, we need to check if the list of NFA states already exists as a DFA state.
    let mutable dfaStateTable : (DFAState * state list) list = []
    let mutable count = 0

    let rec SearchDfaTable(dfaState : DFAState, dfaTable : (DFAState * state list) list) : DFAState option  =
        match dfaTable with
        | (recordedDfaState, nfaLst)::tail ->
            if (dfaState.states = nfaLst) then
                Some recordedDfaState
            else
                SearchDfaTable(dfaState, tail)
        | _ ->
            None

    let rec RecordDFAState(dfaState : DFAState) : DFAState option =
        let res = SearchDfaTable(dfaState, dfaStateTable)
        if (res = None) then 
            dfaState.Name <- count
            count <- count + 1
            dfaStateTable <- (dfaState, dfaState.states)::dfaStateTable
        res



    let mutable epsilonRecord = []
    let rec SearchEpsilonRecord(record : state, epsilonTable : state list) : state option=
        match epsilonTable with
        | head::tail -> 
            if (record = head) then 
                    Some record
            else
                SearchEpsilonRecord(record, tail)
        | [] -> 
            epsilonRecord <- record::epsilonRecord
            None
        
    //Given a DFA state, return list of available DFA states
    //Also updates given DFAs transition list
    //If the given DFAs transition list is already set, it is returned instead
    let Closure(dfaState : DFAState) : DFATransitionList =
        match RecordDFAState(dfaState) with
        | None ->
            let dfaTranLst : DFATransitionObj = DFATransitionObj()
    
            //Add transitions of NFAstates
            for state in dfaState.states do
                for transition in state.transitionLst do
                    dfaTranLst.AddTransition(transition)
    
            //Replace DFA's transitions, with unique transitions of NFAstates
            dfaState.Trans <- dfaTranLst.transes
            dfaTranLst.transes
        | Some dfa -> 
            dfaState.Trans <- dfa.Trans
            dfaState.Name <- dfa.Name
            dfa.Trans

    //Follows DFA transitions
    let rec TraceDFA(dfaState : DFAState) =
        let dfaTrans = Closure(dfaState)

        // (NFATerminal * DFAState)
        for (terminal, newdfaState) in dfaTrans do
            match terminal with
            | Epsilon s -> 
                Closure(newdfaState) |> ignore
            | _ ->
                TraceDFA(newdfaState)
    

    //Code to run NFAToDFA
    let initailDFA = DFAState(nfaState)
    TraceDFA(initailDFA)
    initailDFA




let NFAToDFAOutEps(nfaState : state) : DFAState=
    //To have DFA states that point to the same state, we need to check if the list of NFA states already exists as a DFA state.
    let mutable dfaStateTable : (DFAState * state list) list = []
    let mutable count = 0

    let rec SearchDfaTable(dfaState : DFAState, dfaTable : (DFAState * state list) list) : DFAState option  =
        match dfaTable with
        | (recordedDfaState, nfaLst)::tail ->
            if (dfaState.states = nfaLst) then
                Some recordedDfaState
            else
                SearchDfaTable(dfaState, tail)
        | _ ->
            None

    let rec RecordDFAState(dfaState : DFAState) : DFAState option =
        let res = SearchDfaTable(dfaState, dfaStateTable)
        if (res = None) then 
            dfaState.Name <- count
            count <- count + 1
            dfaStateTable <- (dfaState, dfaState.states)::dfaStateTable
        res



    
    let Closure3(dfaState : DFAState) =
        let mutable epsilonRecord = []

        let rec SearchEpsilonRecord(record : state, epsilonTable : state list) : state option=
            match epsilonTable with
            | head::tail -> 
                if (record = head) then 
                        Some record
                else
                    SearchEpsilonRecord(record, tail)
            | [] -> 
                epsilonRecord <- record::epsilonRecord
                None

        match RecordDFAState(dfaState) with
        | None -> 
            let dfaTranLst = DFATransitionObj()

            let rec AddStateTransitions(nfastat : state) =
                for (transition : state * NFATerminal) in nfastat.transitionLst do
                    match transition with
                    | (epsilonState, Epsilon name) -> 
                        match SearchEpsilonRecord(epsilonState, epsilonRecord) with
                        | None -> AddStateTransitions(epsilonState)
                        | _ -> ()
                    | _ -> dfaTranLst.AddTransition(transition)

            for state in dfaState.states do
                AddStateTransitions(state)

            dfaState.Trans <- dfaTranLst.transes
            dfaTranLst.transes
        | Some dfa -> 
            dfaState.Trans <- dfa.Trans
            dfa.Trans


    let mutable VisitedStates : int list = []

    //Follows DFA transitions
    let rec TraceDFA(dfaState : DFAState) =
        let dfaTrans = Closure3(dfaState)
        
        VisitedStates <- (dfaState.Name)::VisitedStates

        // (NFATerminal * DFAState)
        for (terminal, newdfaState) in dfaTrans do
            match (List.contains newdfaState.Name VisitedStates) with
            | false -> TraceDFA(newdfaState)
            | _ -> ()
    

    //Code to run NFAToDFA
    let initailDFA = DFAState(nfaState)
    TraceDFA(initailDFA)
    initailDFA


let rec PrintDFA(dfaState : DFAState, depth : int) =
    for (terminal, nextdfa) in dfaState.Trans do
        for i = 1 to depth do
            printf "  "
        match terminal with
        | CaseCons -> 
            printfn "%A, states: %A" terminal dfaState.states
        | _ -> 
            printfn "%A" terminal
        PrintDFA(nextdfa, depth + 1)