module RobDFA

open RobNFA
open AbSyn

/// <summary>
/// Collects reductions from a list of NFA states in a new list.
/// </summary>
let rec FilterReductions(stateList : state list) =
    match stateList with
    | state::tail -> 
        match state.reducing with
        | Some reduction -> reduction::FilterReductions(tail)
        | None -> FilterReductions(tail)
    | _ -> []


/// <summary>
/// DFA state as an object
/// </summary>
type DFAState(statelst: state list, name : int) =
    let mutable reducing = 
        let allReductions = FilterReductions(statelst)
        List.distinct allReductions

    //This first gets set in Closure(), as though it could be calculated here
    //the states which are pointed to cannot be existing states, as that would require 
    //the states to be aware of each other
    let mutable TransitionList : (GrammarTerminal * DFAState) list = []

    member this.Name = name
    member this.States = statelst
    member this.Reducing = reducing

    member this.Trans
        with get() = TransitionList
        and set(transLst) = TransitionList <- transLst

    override this.ToString() = ("DFA-state, " + this.Name.ToString())

type DFATransition = GrammarTerminal * DFAState


/// <summary>
/// Used to remove epsilon transitions
/// </summary>
let epsilonUnfold(transLst : (state * NFATerminal) list) =
    let mutable EpsilonedList : state list = []
    let mutable newTransList : (state * GrammarTerminal) list = []

    let rec epsUnfold(tLst : (state * NFATerminal) list) =
        match tLst with
        | (nfaState, Epsilon s)::tail ->
            if not (List.contains (nfaState) EpsilonedList) then
                EpsilonedList <- nfaState::EpsilonedList
                epsUnfold(nfaState.transitionLst)
            epsUnfold(tail)
        | (nfaState, Terminal term)::tail -> 
            //Duplicates are fine, becuase, List.Distinct is used after
            newTransList <- (nfaState, term)::newTransList
            epsUnfold(tail)
        | _ -> 
            ()

    epsUnfold(transLst)
    newTransList

/// <summary>
/// Translates an NFA into a DFA
/// </summary>
let NFAToDFARS(nfaState : state) : DFAState =
    let mutable dfaTable : DFAState list= []
    let mutable dfaCounter = 0;
    let mutable DoneDfas : int list = []


    /// <summary>
    /// Calculates the transitions of a DFA state
    /// </summary>
    let rec Closure(dfaState : DFAState) =
        let allTrans : (state * GrammarTerminal) list = 
            List.fold 
                (fun (acc : (state * GrammarTerminal) list) (innerNfaState : state) -> 
                    epsilonUnfold(innerNfaState.transitionLst) @ acc
                    //innerNfaState.transitionLst @ acc
                ) [] (dfaState.States)
        
        let distinctTrans = List.distinct allTrans
        let dfaTransitions = List.groupBy (fun (stat : state, terminal) -> terminal) distinctTrans
        
        //Remove groupBy duplication
        let CleanedTransitions = 
            List.map (fun (term, transLst) -> 
                (term, List.map (fun (stat, _) -> stat) transLst)
            ) dfaTransitions
        
        //Set given dfa's transitions
        dfaState.Trans <-
            List.map
                (fun (term, stateLst) -> 
                    // Check if list of states are already recorded as a DFAState
                    match (List.tryFind (fun (dfa : DFAState) -> dfa.States = stateLst) dfaTable) with
                    | Some knownDfa -> (term, knownDfa)
                    | _ -> 
                        dfaCounter <- dfaCounter + 1 
                        let newDFA = DFAState(stateLst, dfaCounter)
                        dfaTable <- newDFA::dfaTable
                        TraceDFA(newDFA)
                        (term, newDFA)
                ) CleanedTransitions

    and TraceDFA(dfaState : DFAState) : unit =
        if not (List.contains (dfaState.Name) DoneDfas) then
            Closure(dfaState)
            DoneDfas <- dfaState.Name::DoneDfas
        else
            ()


    //Code to run NFAToDFA
    let initailDFA = DFAState([nfaState], dfaCounter)
    dfaTable <- initailDFA::dfaTable
    TraceDFA(initailDFA)
    initailDFA


/// <summary> 
/// Prints a DFA
/// </summary> 
/// <param name ="nfastate"> starting state of DFA </param>
/// <param name ="depth"> Initail depth, please set to 0 </param>
/// <param name ="nfastate"> Max depth to search through DFA states </param>
let rec PrintDFA(dfaState : DFAState, depth : int, max : int) =
    if (depth < max) then
        for (terminal, nextdfa) in dfaState.Trans do
            for i = 1 to (depth) do printf "  "
            printfn "%A -> %A -> %A" dfaState.Name terminal nextdfa.Name
            PrintDFA(nextdfa, depth + 1, max)