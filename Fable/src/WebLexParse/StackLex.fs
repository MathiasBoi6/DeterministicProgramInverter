module StackLex

open AbSyn

let Lex(chars : char list) : string list =
    let mutable recordedWord = ""
    let resList = 
        List.fold (fun (acc : string list) charC ->
            if System.Char.IsLetter(charC) then
                recordedWord <- recordedWord + string charC
                acc
            else
                let rW = recordedWord
                recordedWord <- ""
                match charC with
                | '(' -> 
                    "("::rW::acc
                | ')' -> 
                    ")"::rW::acc
                | ',' -> 
                    ","::rW::acc
                | ';' -> 
                    ";"::rW::acc
                | x ->
                    if System.Char.IsNumber(x) then
                        string x::rW::acc
                    else
                        rW::acc
        ) [] chars
    List.rev resList |> List.filter (fun strig -> not (System.String.IsNullOrWhiteSpace(strig)))

let FindParenthesisMatch(lst : string list) : int option =
    let mutable depth = 0;

    let mutable i = 0
    let mutable res = None
    while (i < lst.Length && res = None) do
        match lst.Item(i) with
        | "(" -> depth <- depth + 1
        | ")" -> 
            depth <- depth - 1
            if (depth = 0) then
                res <- Some i
        | _ -> ()
        i <- i + 1
    res

let rec Parse(stringList : string list, acc : Value list) : Value list =
    match stringList with
    | head::tail ->
        if (Seq.forall (fun c -> System.Char.IsLetter(c)) head) then
            if List.tryHead(tail) = Some "(" then
                let matchOption = FindParenthesisMatch(tail)
                match matchOption with
                | Some matchIndex ->
                    if (matchIndex + 1 < tail.Length) then
                        let parseRes = Parse(tail.[..matchIndex + 1], [])
                        Parse(tail.[matchIndex + 1..], acc @ [CONS (head, parseRes)])
                    else
                        let parseRes = Parse(tail.[..matchIndex], [])
                        acc @ [CONS (head, parseRes)]
                | _ -> failwith "Parentheses did not match"
            else Parse(tail, acc @ [CONS (head, [])])
        else 
            match System.Int32.TryParse head with
            | true, num -> 
                Parse(tail, acc @ [NUM num])
            | false, _ -> Parse(tail, acc)
    | [] -> acc

let LexStack(stack : string) =
    let cleanStack = stack.Trim()
    
    let tokenList = Lex(Seq.toList cleanStack)
    let parsedStack = Parse(tokenList, [])
    parsedStack
