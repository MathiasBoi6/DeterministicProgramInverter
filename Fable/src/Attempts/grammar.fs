module grammar

open AbSyn

let RPS : GrammarProgram =
    [
    "RPS", 
        [
            [CaseEquality; Case "RPS"; Nonterminal "RemoveSymbol"; MakeNum 0];
            [CaseNonEquality; Case "RPS"; Nonterminal "ResolveSigns"]
        ];
    "ResolveSigns",
        [
            [Case "Rock"; Case "Scissor"; MakeNum 1];
            [Case "Rock"; Case "Paper"; MakeNum -1];
            [Case "Scissor"; Case "Rock"; MakeNum -1];
            [Case "Scissor"; Case "Paper"; MakeNum 1];
            [Case "Paper"; Case "Rock"; MakeNum 1];
            [Case "Paper"; Case "Scissor"; MakeNum -1]
        ];
    "RemoveSymbol",
        [
            [Case "Rock"];
            [Case "Scissor"];
            [Case "Paper"]
        ];
    "NeverRun",
        [  
            [Make ("RPS", 2); Make ("Rock", 0); Make ("Paper", 0); Make ("Scissor", 0)]
        ]
    ]






let GrammarString(gram) : string =
    let mutable strig = "";
    for (nt, prods) in gram do
        strig <- strig + sprintf "%A:\n" nt 
        for x in prods do
            strig <- strig + sprintf "  %A\n" x 
    strig



