module TestFunctions

open AbSyn

/// <summary> Given the path to a folder gathers a list of files with the extensions 
/// .stackInp and .stackExp, along with a single file ending in .fct </summary>
/// <returns> A tuple: (fct file path, stackInp file path list, stackExp file path list) </returns>
let SplitFiles(path : string) =
    let files = System.IO.Directory.GetFiles(path)
    let mutable fct : string = ""
    let mutable stackInp : string list = []
    let mutable stackExp : string list = []

    for file in files do
        match (System.IO.Path.GetExtension(file)) with
        | ".fct" ->         fct <- file
        | ".stackInp" ->    stackInp <- file::stackInp
        | ".stackExp" ->    stackExp <- file::stackExp
        | _ -> ()

    (fct, stackInp, stackExp)

/// <summary>
/// Prints a grammar program to a file
/// </summary>
/// <param name="grammar"> Grammar program to be printed</param>
/// <param name="sw"> A StreamWriter which the grammar program is saved to </param>
let TestPrintGrammar(grammar : GrammarProgram, sw : System.IO.StreamWriter) : unit =
    for (nt, prods) in grammar do
        sprintf "%A:" nt |> sw.WriteLine
        for x in prods do
            sprintf "  %A" x |> sw.WriteLine

/// <summary>
/// Prints test of stack. 
/// </summary>
/// <param name="inp"> Stack file which is tested </param>
/// <param name="out"> Actual output of stack </param>
/// <param name="exp"> Expected output of stack </param>
/// <param name="dir"> A char, to determine whether forwards or backwards computation is done </param>
let PrintTest(inp : string, out, exp, dir) : unit =
    let simpleInp = inp.LastIndexOf('\\') + 1 |> inp.Substring
    if (out = exp) then
        match dir with
        | 'f' ->
            printfn "Correct forward computation, %s" simpleInp
        | _ ->
            printfn "Correct backward computation, %s" simpleInp
    else
        let defaultColor = System.Console.ForegroundColor
        System.Console.ForegroundColor <- (System.ConsoleColor.Red)
        printfn ">>>>>>>>>>>>>>>>>>>>>>>>>>"
        match dir with
        | 'f' ->
            printfn "INCORRECT forward computation, %s" simpleInp
        | _ ->
            printfn "INCORRECT backward computation, %s" simpleInp
        printfn "Expected: %s" (Interpreter.PPStack(exp))
        printfn "Received: %s" (Interpreter.PPStack(out))
        printfn "<<<<<<<<<<<<<<<<<<<<<<<<<<\n"
        System.Console.ForegroundColor <- defaultColor



/// <summary>
/// Tests forward and backward computation of all files in Test directory. 
/// </summary>
/// <sideeffects> 
/// Creates .gram, .invgram and .stackOut file for each test
/// </sideeffects> 
let TestFCTFiles() =
    let directories = System.IO.Directory.EnumerateDirectories("Tests")
    for dir in directories do
        printfn "\n\nTesting Directory: %A" dir
        let fct, stackInps, stackExps = SplitFiles(dir)
        let dirName = dir.LastIndexOf('\\') + 1 |> dir.Substring
            
        //Load grammar
        let grammar = GrammarParser.LexParseAndGenGrammar(fct)
        use sw = System.IO.File.CreateText(dir + "/" + dirName + ".gram");
        TestPrintGrammar(grammar, sw)
        sw.Close()

        //Invert grammar
        let locInvGrammar = LocalInverter.LocalInversion(grammar)
        let nfa = RobNFA.CreateNFARS(locInvGrammar).Value
        let dfa = RobDFA.NFAToDFARS(nfa)
        let collectionOfPPs = RobParse.CalculateProgramPointsInit(dfa)
        let uncompressedGrammar = RobParse.GenerateGrammar(collectionOfPPs)
        let invertedGrammar = RobParse.TransitionCompression(uncompressedGrammar)
        use sw = System.IO.File.CreateText(dir + "/" + dirName + ".Invgram");
        TestPrintGrammar(invertedGrammar, sw)
        sw.Close()


        //Interpret stacks
        try
            List.iter2 (fun (inp : string) exp ->
                printfn ""
                let inpNameLength : int = inp.LastIndexOf('.')
                let inpName = inp.Substring(0, inpNameLength)

                //Load stack
                let sIn = StackLex.LexParseStack(inp)
                let sEx = StackLex.LexParseStack(exp)

                //Interpret stack
                let sOu =
                    try Some (Interpreter.RunGrammar(grammar, sIn))
                    with Failure msg -> 
                        let defaultColor = System.Console.ForegroundColor
                        System.Console.ForegroundColor <- (System.ConsoleColor.Red)
                        printfn "Forwards Computation Error: \n%A" msg
                        System.Console.ForegroundColor <- defaultColor
                        None

                match sOu with
                | Some valueS ->
                    PrintTest(inp, valueS, sEx, 'f')

                    let outFile = inpName + ".stackOut"
                    use sw = System.IO.File.CreateText(outFile);
                    sw.WriteLine(Interpreter.PPStack(valueS))
                    sw.Close()
                
                    //Interpret inverse
                    try 
                        let reInp = Interpreter.RunGrammar(invertedGrammar, valueS)
                        PrintTest(outFile, reInp, sIn, 'b')
                    with Failure msg -> 
                        let defaultColor = System.Console.ForegroundColor
                        System.Console.ForegroundColor <- (System.ConsoleColor.Red)
                        printfn "Backward Computation Error: \n%A" msg
                        System.Console.ForegroundColor <- defaultColor
                | None -> ()
                ) stackInps stackExps
        with Failure msg -> failwith msg
    printfn ""