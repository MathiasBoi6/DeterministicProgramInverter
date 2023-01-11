//module GrammarInverter

//open System

type Terminal =
    | CaseCons
    | MakeCons
    | MakeNum of int
    | CaseNum of int
    | Nonterminal of int


type Production = Terminal list

type Nonterminal = int * Production list

type GrammarProgram = Nonterminal list


let InvertTerminal(terminal : Terminal) : Terminal =
    match terminal with 
    | CaseCons -> MakeCons
    | CaseNum n -> MakeNum n
    | MakeCons -> CaseCons
    | MakeNum n -> CaseNum n
    | x -> x

let InvertGrammar(grammar : GrammarProgram) : GrammarProgram =
    List.map 
        (fun (nterm, productionLst : Production list) -> 
            nterm, List.map
                (fun terminalLst -> 
                    List.rev  (List.map (fun terminal -> InvertTerminal(terminal)) terminalLst)
                ) productionLst
        ) grammar


let inc : GrammarProgram = 
    [0, 
        [
            [CaseNum 1; MakeNum 1; MakeNum 0; MakeCons]; 
            [CaseCons; CaseNum 0; MakeNum 1; MakeCons]; 
            [CaseCons; CaseNum 1; Nonterminal 0; MakeNum 0; MakeCons]
        ]
    ]

let inc1  : GrammarProgram = 
    [0, 
        [
            [CaseCons; CaseNum 0; CaseNum 1; MakeNum 1]; 
            [CaseCons; CaseNum 1; MakeNum 0; MakeCons]; 
            [CaseCons; CaseNum 0; Nonterminal 0; MakeNum 1; MakeCons]
        ]
    ]

(*
let LeftFactor(grammar : GrammarProgram) : GrammarProgram =
    let test =
        List.map 
            (fun _ -> _
            ) grammar

    grammar

let LeftFaktorAndUnFold()=
    
    printf "hej"
*)