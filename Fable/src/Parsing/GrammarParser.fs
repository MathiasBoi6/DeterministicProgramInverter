module GrammarParser

open AbSyn
open FSharp.Text.Lexing

/// <summary>
/// Lexes, parses and performs code generation for given file,
/// translating the file to a grammar program.
/// </summary>
let LexParseAndGenGrammar(filename : string) : GrammarProgram =
    use textReader = new System.IO.StreamReader(filename)
    let lexbuf = LexBuffer<char>.FromTextReader textReader
    
    let resFromParser =
        try
            Parser.parseProg Lexer.tokenstream lexbuf 
        with
            | Failure msg -> 
                let endPos = lexbuf.EndPos
                let startPos = lexbuf.StartPos
                printfn "%A" msg
                failwithf "Error between (row: %A, column: %A) and (row: %A, column: %A) in file %A" 
                    (startPos.Line + 1) (startPos.Column + 1) (endPos.Line + 1) (endPos.Column + 1) filename

    //printfn "\n%A\n" resFromParser
    try 
        CodeGenerator.GenerateGrammar(resFromParser)
    with
        | Failure msg -> 
            printfn "\nError in code generation (Variable allocation)."
            failwithf "%A" msg


/// <summary>
/// Loacally inverts grammar, Calculates LR items (DFA), collection of program points,
/// Uncompressed grammar, and then compresses that grammar.
/// </summary>
let InvertGrammar(grammar) =
    let locInvGrammar = LocalInverter.LocalInversion(grammar)

    let nfa = RobNFA.CreateNFARS(locInvGrammar).Value
    let dfa = RobDFA.NFAToDFARS(nfa)

    let collectionOfPPs = RobParse.CalculateProgramPointsInit(dfa)
    let uncompressedGrammar = RobParse.GenerateGrammar(collectionOfPPs)
    RobParse.TransitionCompression(uncompressedGrammar)