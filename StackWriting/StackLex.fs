module StackLex

open System.IO
open FSharp.Text.Lexing

let LexParseStack(filename : string) =
    use textReader = new System.IO.StreamReader(filename)
    let lexbuf = LexBuffer<char>.FromTextReader textReader
    
    
    let resFromParser =
        try
            StackParser.parseStack StackLexer.tokenstream lexbuf 
        with
            | Failure msg -> 
                let endPos = lexbuf.EndPos
                let startPos = lexbuf.StartPos
                printfn "%A" msg
                failwithf "Error between (row: %A, column: %A) and (row: %A, column: %A) in file %A" 
                    (startPos.Line + 1) (startPos.Column + 1) (endPos.Line + 1) (endPos.Column + 1) filename
    
    resFromParser