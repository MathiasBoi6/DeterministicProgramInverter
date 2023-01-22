open AbSyn
open FSharp.Text.Lexing



/// <summary> Uses functions from inversion project based on command line arguments <\summary>
[<EntryPoint>]
let main (paramList: string[]) : int =
    match paramList with
    | [|this; "-LexStack"; stackFile|] ->
        if (System.IO.File.Exists(stackFile)) then
            printfn "Lexing stack file: %A\n" stackFile
            let stack = StackLex.LexParseStack(stackFile)

            printfn "\nStack was: %s" (Interpreter.PPStack(stack))
        else
            failwithf "Could not find file %A" stackFile
    | [|this; "-Invert"; file|] -> 
        if (System.IO.File.Exists(file)) then
            printfn "Inverting file: %A\n" file

            let grammar = GrammarParser.LexParseAndGenGrammar(file)
            printfn "Lexing complete"

            let locInvGrammar = LocalInverter.LocalInversion(grammar)
            printfn "Local inversion complete"

            let nfa = RobNFA.CreateNFARS(locInvGrammar).Value
            let dfa = RobDFA.NFAToDFARS(nfa)
            printfn "Translation to NFA and DFA complete"


            let collectionOfPPs = RobParse.CalculateProgramPointsInit(dfa)
            let uncompressedGrammar = RobParse.GenerateGrammar(collectionOfPPs)
            printfn "Calculation of uncompressed grammar complete"

            let invertedGrammar = RobParse.TransitionCompression(uncompressedGrammar)
            
            printfn "\nGrammar became:"
            RobParse.PrintGrammar(invertedGrammar)
        else
            failwithf "Could not find file %A" file
    | [|this; "-InvertComplete"; file|] -> 
        if System.IO.File.Exists(file) then
            printfn "Lexing file: %A\n" file

            let grammar = GrammarParser.LexParseAndGenGrammar(file)
            printfn "Lexing complete"

            let locInvGrammar = LocalInverter.LocalInversion(grammar)
            printfn "Local inversion complete"

            let nfa = RobNFA.CreateNFARS(locInvGrammar).Value
            let dfa = RobDFA.NFAToDFARS(nfa)
            printfn "Translation to NFA and DFA complete"

            let collectionOfPPs = RobParse.CalculateProgramPointsInit(dfa)

            match RobParse.GenerateGrammarComplete(collectionOfPPs) with
            | fs, gs, hs ->
                printfn "Calculation of uncompressed grammar complete\n"
                printfn "HS = %A" hs
                let uncompressedGrammar = (fs, gs)
        
                RobParse.PrintUncompressedGrammar(uncompressedGrammar)
                let invertedGrammar = RobParse.TransitionCompression(uncompressedGrammar)
                
                printfn "\nGrammar became:"
                RobParse.PrintGrammar(invertedGrammar)
        else
            failwithf "Could not find file %A" file
    | [|this; "-DoubleInvert"; file|] -> 
        if (System.IO.File.Exists(file)) then
            printfn "Doubly Inverting file: %A\n" file

            let grammar = GrammarParser.LexParseAndGenGrammar(file)
            printfn "Lexing complete"

            let inverseGram = GrammarParser.InvertGrammar(grammar)
            printfn "First inversion complete"
            let reInverseGram = GrammarParser.InvertGrammar(inverseGram)
            printfn "Second inversion complete"
            
            printfn "\nOriginal grammar was:"
            RobParse.PrintGrammar(grammar)

            printfn "\n\nGrammar Became:"
            RobParse.PrintGrammar(reInverseGram)
        else
            failwithf "Could not find file %A" file
    | [|this; "-Interpret"; file; stackFile|] -> 
        if (System.IO.File.Exists(file) && System.IO.File.Exists(stackFile)) then
            printfn "Lexing file: %A\n" file

            let grammar = GrammarParser.LexParseAndGenGrammar(file)

            let stack = StackLex.LexParseStack(stackFile)
            printfn "Lexing complete"
            RobParse.PrintGrammar(grammar)

            printfn "\n%s" (Interpreter.PrettyPrintStack(grammar, stack))
        else
            failwithf "Could not find file %A or %A" file stackFile
    | [|this; "-Test"; "fct"|] -> 
        TestFunctions.TestFCTFiles()

    | [|this; file; stackFile|] -> 
        if (System.IO.File.Exists(file) && System.IO.File.Exists(stackFile)) then
            printfn "Lexing file: %A\n" file

            let grammar = GrammarParser.LexParseAndGenGrammar(file)
            let stack = StackLex.LexParseStack(stackFile)
            printfn "Lexing complete"

            let locInvGrammar = LocalInverter.LocalInversion(grammar)
            printfn "Local inversion complete"

            let nfa = RobNFA.CreateNFARS(locInvGrammar).Value
            let dfa = RobDFA.NFAToDFARS(nfa)
            printfn "Translation to NFA and DFA complete"

            let collectionOfPPs = RobParse.CalculateProgramPointsInit(dfa)
            let uncompressedGrammar = RobParse.GenerateGrammar(collectionOfPPs)
            printfn "Calculation of uncompressed grammar complete"

            let invertedGrammar = RobParse.TransitionCompression(uncompressedGrammar)
            printfn "Grammar generated"

            printfn "\n%s" (Interpreter.PrettyPrintStack(invertedGrammar, stack))
        else
            failwithf "Could not find file %A or %A" file stackFile
    | _ -> 
        printfn "No file or options was applied"
    0