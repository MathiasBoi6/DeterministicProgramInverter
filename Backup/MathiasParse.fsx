#r @"NFADefiner.dll"
#r @"DFADefiner.dll"

open GrammarToNFA
open NFAToDFA


let inc1 : GrammarProgram = 
    ["inc", 
        [
            [CaseCons; CaseNum 0; CaseNum 1; MakeNum 1]; 
            [CaseCons; CaseNum 1; MakeNum 0; MakeCons]; 
            [CaseCons; CaseNum 0; Nonterminal "inc"; MakeNum 1; MakeCons]
        ]
    ]

let snoc : GrammarProgram = 
    ["snoc", 
        [
            [CaseCons; MakeNum 21; CaseNum 0; MakeNum 0];  
            [CaseCons; MakeNum 21; Nonterminal "snoc"; MakeNum 312; MakeCons]
        ]
    ]

let reverse : GrammarProgram = 
    [("reverse", 
        [
            [Nonterminal "rev"; MakeNum 21; CaseNum 0]
        ]);
    ("rev", 
        [
            [MakeNum 0];  
            [Nonterminal "rev"; MakeNum 21; CaseCons; MakeNum 132; MakeCons]
        ])
    ]

let rec searchDfaTable(dfa : DFAState, dfaTable : DFAState list) =
    match dfaTable with
    | dfaHead::tail ->
        if (dfa = dfaHead) then
            Some dfa
        else
            searchDfaTable(dfa, tail)
    | _ -> 
        None

//Prints pseudo-DFA and only follows each epsilon binding once
let rec PrtinDfa3(dfaState : DFAState, depth : int, dfaTable : DFAState list) =
    for (terminal, nextdfa) in dfaState.Trans do
        for i = 1 to depth do
            printf "  "
        printfn "%A, next %A" terminal nextdfa.Name
        match terminal with
        | Epsilon s ->
            match searchDfaTable(nextdfa, dfaTable) with
            | Some dfa -> printf ""
            | None ->
                PrtinDfa3(nextdfa, depth + 1, nextdfa::dfaTable)
        | _ -> 
            PrtinDfa3(nextdfa, depth + 1, dfaTable)

    







type Item = DFAState * int
type ProdItem =
    | F of Item * NFATerminal * Item
    | F2 of Item * NFATerminal
    | G of Item * NFATerminal * Item

let mutable specialLst : ProdItem list = []
let mutable specialLstG : ProdItem list = []

let rec searchSpec(proditem : ProdItem, prodItemList : ProdItem list) : ProdItem option =
    match prodItemList with
    | head::tail -> 
        if (head = proditem) then
            Some head
        else
            searchSpec(proditem, tail)
    | _ -> None

let AddF(dfaItem1 : Item, terminal : NFATerminal, dfaItem2 : Item) =
    let mutable fProd = F (dfaItem1, terminal, dfaItem2)
    if ((fst dfaItem2).Trans = []) then 
        fProd <- F2 (dfaItem1, terminal)
        
    match searchSpec(fProd, specialLst) with
    | None -> 
        specialLst <- fProd::specialLst
    | Some _ -> () //Item transition already added
    

let AddG(dfaItem1 : Item, terminal : NFATerminal, dfaItem2 : Item) =
    let mutable gProd = G (dfaItem1, terminal, dfaItem2)
    if (List.contains (gProd) specialLstG = false) then 
        specialLstG <- gProd::specialLstG


//Prints pseudo-DFA and only follows each epsilon binding once
//When creating the gramar, we want to know which was the last terminal of the dropped stack and to continue we need to know the state, therefore the type of the stack.
let rec ExhastWitEp(dfaState : DFAState, stack : (DFAState * NFATerminal) list, dfaTable : DFAState list) =
    for (terminal, nextdfa) in dfaState.Trans do
        printfn "%A, stack: %A" terminal stack
        match terminal with
        | Epsilon s ->
            match searchDfaTable(nextdfa, dfaTable) with
            | Some dfa -> () //Dont follow
            | None ->
                AddF(Item (dfaState, dfaState.Name), terminal, Item (nextdfa, nextdfa.Name))
                ExhastWitEp(nextdfa, (dfaState, terminal)::stack, nextdfa::dfaTable)
        | Nonterminal _ -> () //Dont follow
        | _ -> 
            AddF(Item (dfaState, dfaState.Name), terminal, Item (nextdfa, nextdfa.Name))
            ExhastWitEp(nextdfa, (dfaState, terminal)::stack, dfaTable)
    
    for (nonTerm, reduceAmount) in dfaState.Reducing do
        let mutable reducedStack = stack.[reduceAmount..]
        if not (reducedStack.Length = 0) then
            let mutable (reducedState, reducingTerminal) = reducedStack.Head
            match reducingTerminal with 
            | Epsilon _ -> 
                reducingTerminal <- snd (stack.Item(reduceAmount - 1))
                reducedStack <- reducedStack.Tail
            | _ -> 
                failwith "CAN THIS RUN?????"
                //When reducing to a state without a epsilon transition
                () //Dont follow


            //printfn "Reducing by %A, redTerm %A, new length %A" reduceAmount reducingTerminal reducedStack.Length
            //printfn "Reduced's transes: %A\n" reducedState.Trans
            
            for (terminal, nextdfa) in reducedState.Trans do
                match terminal with
                | Nonterminal s -> //Goto
                    if (s = nonTerm) then
                        printfn "\n------------FromState: %A, shift: %A AfterState: %A" reducedState.Name reducingTerminal nextdfa.Name
                        printfn "Followed on nonterm %A\n" nonTerm
                        AddG(Item(reducedState, reducedState.Name), reducingTerminal, Item(nextdfa, nextdfa.Name))
                        ExhastWitEp(nextdfa, (dfaState, terminal)::reducedStack, dfaTable) //State is not added to stack, as it is the current head.
                | _ -> 
                    () //Dont follow
        else
            printfn "Ret to start\n"





//Prints pseudo-DFA and only follows each epsilon binding once
//When creating the gramar, we want to know which was the last terminal of the dropped stack and to continue we need to know the state, therefore the type of the stack.
let rec Exhast1(dfaState : DFAState, stack : (DFAState * NFATerminal) list, backTable : (int * int) list) =
    //Handle Shifts
    for (terminal, nextdfa) in dfaState.Trans do
        if not (List.contains (nextdfa.Name, dfaState.Name) backTable) then 
            printfn "%A, stack: %A" terminal stack
            match terminal with
            | Epsilon s ->
                failwith "Found Epsilon shift in DFA"
            | Nonterminal _ -> () //Dont follow
            | _ -> 
                AddF(Item (dfaState, dfaState.Name), terminal, Item (nextdfa, nextdfa.Name))
                if (nextdfa.Name < dfaState.Name) then
                    Exhast1(nextdfa, (dfaState, terminal)::stack, (nextdfa.Name, dfaState.Name)::backTable)
                else
                    Exhast1(nextdfa, (dfaState, terminal)::stack, backTable)
    
    //Handle Reductions
    for (nonTerm, reduceAmount) in dfaState.Reducing do
        let mutable reducedStack = stack.[reduceAmount..]
        if not (reducedStack.Length = 0) then
            let mutable (reducedState, reducingTerminal) = reducedStack.Head
            match reducingTerminal with 
            | Epsilon _ -> 
                reducingTerminal <- snd (stack.Item(reduceAmount - 1))
                reducedStack <- reducedStack.Tail
            | _ -> 
                printfn "???__???UNREACHABLE CODE???__???"

            //printfn "Reducing by %A, redTerm %A, new length %A" reduceAmount reducingTerminal reducedStack.Length

            //printfn "Reduced's transes: %A\n" reducedState.Trans
            


            for (terminal, nextdfa) in reducedState.Trans do
                match terminal with
                | Nonterminal s ->
                    if (s = nonTerm) then
                        printfn "------------FromState: %A, shift: %A AfterState: %A" dfaState.Name reducingTerminal nextdfa.Name
                        printfn "Followed on nonterm %A" nonTerm
                        Exhast1(nextdfa, (dfaState, terminal)::reducedStack, backTable) //State is not added to stack, as it is the current head.
                | _ -> 
                    () //Dont follow
        else
            printfn "Ret to start\n"




//let nfa = CreateNFAwitEps(inc1).Value
//let nfa = CreateNFAwitEps(snoc).Value
let nfa = CreateNFAwitEps(reverse).Value

let dfa = NFAToDFA(nfa)
//let dfa = NFAToDFAOutEps(nfa)




//Exhast1(dfa, [], [])
(*
ExhastWitEp(dfa, [], [])
printfn "\n\n" 
List.map (fun tran -> printfn "%A" tran) specialLst
printfn "\n\n" 
List.map (fun tran -> printfn "%A" tran) specialLstG
*)

//PrtinDfa3(dfa, 0, [])






//Outlines some of NFA and DFA
List.map 
    (fun (tran : state * NFATerminal)-> 
    printfn "%A" tran
    List.map 
        (fun (tran2 : state * NFATerminal) -> 
        printfn "  %A" tran2
        List.map 
            (fun (tran3 : state * NFATerminal) -> 
            printfn "    %A" tran3
            ) (fst tran2).transitionLst
        ) (fst tran).transitionLst
    ) nfa.transitionLst

printfn "\n\n" 

List.map 
    (fun (tran : NFATerminal * DFAState)-> 
    printfn "%A" tran
    List.map 
        (fun (tran2 : NFATerminal * DFAState) -> 
        printfn "  %A" tran2
        List.map 
            (fun (tran3 : NFATerminal * DFAState) -> 
            printfn "    %A" tran3
            List.map 
                (fun (tran4 : NFATerminal * DFAState) -> 
                printfn "      %A" tran4
                ) (snd tran3).Trans
            ) (snd tran2).Trans
        ) (snd tran).Trans
    ) dfa.Trans
