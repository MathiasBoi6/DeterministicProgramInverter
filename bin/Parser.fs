// Implementation file for parser generated by fsyacc
module Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open FSharp.Text.Lexing
open FSharp.Text.Parsing.ParseHelpers
# 1 "Lexing\Parser.fsp"


open AbSyn


# 12 "bin\Parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | EOF
  | VAR of (string * Column)
  | NUM of (int * Column)
  | DEFINE of (Column)
  | LET of (Column)
  | CASE of (Column)
  | NIL of (Column)
  | RETURN of (Column)
  | MAKE of (Column)
  | DUP of (Column)
  | EQ of (Column)
  | LPAR of (Column)
  | RPAR of (Column)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_EOF
    | TOKEN_VAR
    | TOKEN_NUM
    | TOKEN_DEFINE
    | TOKEN_LET
    | TOKEN_CASE
    | TOKEN_NIL
    | TOKEN_RETURN
    | TOKEN_MAKE
    | TOKEN_DUP
    | TOKEN_EQ
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startparseProg
    | NONTERM_parseProg
    | NONTERM_Definitions
    | NONTERM_Nonterminal
    | NONTERM_NamedVar
    | NONTERM_Productions
    | NONTERM_ParameterList
    | NONTERM_MakeNary
    | NONTERM_ProductionList
    | NONTERM_Cases
    | NONTERM_Case

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | VAR _ -> 1 
  | NUM _ -> 2 
  | DEFINE _ -> 3 
  | LET _ -> 4 
  | CASE _ -> 5 
  | NIL _ -> 6 
  | RETURN _ -> 7 
  | MAKE _ -> 8 
  | DUP _ -> 9 
  | EQ _ -> 10 
  | LPAR _ -> 11 
  | RPAR _ -> 12 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_VAR 
  | 2 -> TOKEN_NUM 
  | 3 -> TOKEN_DEFINE 
  | 4 -> TOKEN_LET 
  | 5 -> TOKEN_CASE 
  | 6 -> TOKEN_NIL 
  | 7 -> TOKEN_RETURN 
  | 8 -> TOKEN_MAKE 
  | 9 -> TOKEN_DUP 
  | 10 -> TOKEN_EQ 
  | 11 -> TOKEN_LPAR 
  | 12 -> TOKEN_RPAR 
  | 15 -> TOKEN_end_of_input
  | 13 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startparseProg 
    | 1 -> NONTERM_parseProg 
    | 2 -> NONTERM_Definitions 
    | 3 -> NONTERM_Definitions 
    | 4 -> NONTERM_Nonterminal 
    | 5 -> NONTERM_NamedVar 
    | 6 -> NONTERM_NamedVar 
    | 7 -> NONTERM_Productions 
    | 8 -> NONTERM_Productions 
    | 9 -> NONTERM_Productions 
    | 10 -> NONTERM_Productions 
    | 11 -> NONTERM_Productions 
    | 12 -> NONTERM_Productions 
    | 13 -> NONTERM_Productions 
    | 14 -> NONTERM_Productions 
    | 15 -> NONTERM_Productions 
    | 16 -> NONTERM_Productions 
    | 17 -> NONTERM_ParameterList 
    | 18 -> NONTERM_ParameterList 
    | 19 -> NONTERM_MakeNary 
    | 20 -> NONTERM_MakeNary 
    | 21 -> NONTERM_ProductionList 
    | 22 -> NONTERM_ProductionList 
    | 23 -> NONTERM_Cases 
    | 24 -> NONTERM_Cases 
    | 25 -> NONTERM_Case 
    | 26 -> NONTERM_Case 
    | 27 -> NONTERM_Case 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 15 
let _fsyacc_tagOfErrorTerminal = 13

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | VAR _ -> "VAR" 
  | NUM _ -> "NUM" 
  | DEFINE _ -> "DEFINE" 
  | LET _ -> "LET" 
  | CASE _ -> "CASE" 
  | NIL _ -> "NIL" 
  | RETURN _ -> "RETURN" 
  | MAKE _ -> "MAKE" 
  | DUP _ -> "DUP" 
  | EQ _ -> "EQ" 
  | LPAR _ -> "LPAR" 
  | RPAR _ -> "RPAR" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | VAR _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | NUM _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | DEFINE _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | LET _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | CASE _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | NIL _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | RETURN _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | MAKE _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | DUP _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | EQ _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | LPAR _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | RPAR _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 2us; 65535us; 0us; 2us; 7us; 8us; 1us; 65535us; 4us; 5us; 4us; 65535us; 11us; 12us; 14us; 15us; 40us; 41us; 66us; 67us; 12us; 65535us; 5us; 6us; 17us; 21us; 24us; 56us; 27us; 28us; 30us; 31us; 42us; 43us; 44us; 45us; 47us; 50us; 50us; 50us; 52us; 56us; 56us; 56us; 61us; 62us; 2us; 65535us; 47us; 48us; 50us; 51us; 12us; 65535us; 5us; 36us; 17us; 36us; 24us; 36us; 27us; 36us; 30us; 36us; 42us; 36us; 44us; 36us; 47us; 36us; 50us; 36us; 52us; 36us; 56us; 36us; 61us; 36us; 3us; 65535us; 24us; 25us; 52us; 53us; 56us; 57us; 3us; 65535us; 18us; 19us; 21us; 22us; 63us; 64us; 1us; 65535us; 59us; 60us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 6us; 8us; 13us; 26us; 29us; 42us; 46us; 50us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 1us; 1us; 2us; 2us; 3us; 2us; 2us; 3us; 2us; 2us; 3us; 2us; 2us; 3us; 1us; 2us; 1us; 4us; 1us; 4us; 1us; 4us; 1us; 4us; 1us; 4us; 2us; 5us; 6us; 1us; 5us; 10us; 7us; 8us; 9us; 10us; 11us; 12us; 15us; 16us; 19us; 20us; 2us; 7us; 8us; 2us; 7us; 14us; 1us; 7us; 1us; 7us; 1us; 8us; 1us; 8us; 1us; 8us; 1us; 9us; 1us; 9us; 1us; 9us; 1us; 10us; 1us; 10us; 1us; 10us; 1us; 11us; 1us; 11us; 1us; 11us; 3us; 12us; 19us; 20us; 1us; 12us; 1us; 12us; 1us; 13us; 1us; 14us; 1us; 15us; 1us; 15us; 1us; 15us; 1us; 15us; 1us; 15us; 1us; 15us; 1us; 15us; 1us; 15us; 1us; 15us; 1us; 16us; 1us; 16us; 1us; 16us; 2us; 17us; 18us; 1us; 17us; 2us; 19us; 20us; 1us; 19us; 1us; 19us; 1us; 20us; 2us; 21us; 22us; 1us; 21us; 2us; 23us; 24us; 2us; 23us; 24us; 2us; 23us; 24us; 2us; 23us; 24us; 2us; 23us; 24us; 2us; 23us; 24us; 1us; 23us; 1us; 25us; 2us; 26us; 27us; 1us; 26us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 8us; 11us; 14us; 17us; 20us; 22us; 24us; 26us; 28us; 30us; 32us; 35us; 37us; 48us; 51us; 54us; 56us; 58us; 60us; 62us; 64us; 66us; 68us; 70us; 72us; 74us; 76us; 78us; 80us; 82us; 86us; 88us; 90us; 92us; 94us; 96us; 98us; 100us; 102us; 104us; 106us; 108us; 110us; 112us; 114us; 116us; 118us; 121us; 123us; 126us; 128us; 130us; 132us; 135us; 137us; 140us; 143us; 146us; 149us; 152us; 155us; 157us; 159us; 162us; |]
let _fsyacc_action_rows = 68
let _fsyacc_actionTableElements = [|1us; 32768us; 11us; 4us; 0us; 49152us; 1us; 32768us; 0us; 3us; 0us; 16385us; 1us; 32768us; 3us; 9us; 2us; 32768us; 1us; 37us; 11us; 16us; 1us; 32768us; 12us; 7us; 1us; 16387us; 11us; 4us; 0us; 16386us; 1us; 32768us; 11us; 10us; 1us; 32768us; 1us; 11us; 1us; 32768us; 1us; 14us; 1us; 32768us; 12us; 13us; 0us; 16388us; 1us; 16390us; 1us; 14us; 0us; 16389us; 7us; 32768us; 1us; 47us; 4us; 38us; 5us; 17us; 7us; 24us; 8us; 33us; 9us; 27us; 10us; 30us; 2us; 32768us; 1us; 18us; 11us; 16us; 1us; 16398us; 11us; 58us; 1us; 32768us; 12us; 20us; 0us; 16391us; 1us; 32768us; 11us; 58us; 1us; 32768us; 12us; 23us; 0us; 16392us; 2us; 32768us; 1us; 37us; 11us; 16us; 1us; 32768us; 12us; 26us; 0us; 16393us; 2us; 32768us; 1us; 37us; 11us; 16us; 1us; 32768us; 12us; 29us; 0us; 16394us; 2us; 32768us; 1us; 37us; 11us; 16us; 1us; 32768us; 12us; 32us; 0us; 16395us; 2us; 32768us; 1us; 52us; 2us; 34us; 1us; 32768us; 12us; 35us; 0us; 16396us; 0us; 16397us; 0us; 16398us; 1us; 32768us; 11us; 39us; 1us; 32768us; 11us; 40us; 1us; 32768us; 1us; 14us; 1us; 32768us; 12us; 42us; 2us; 32768us; 1us; 37us; 11us; 16us; 1us; 32768us; 12us; 44us; 2us; 32768us; 1us; 37us; 11us; 16us; 1us; 32768us; 12us; 46us; 0us; 16399us; 2us; 32768us; 1us; 37us; 11us; 16us; 1us; 32768us; 12us; 49us; 0us; 16400us; 2us; 16402us; 1us; 37us; 11us; 16us; 0us; 16401us; 3us; 32768us; 1us; 37us; 11us; 16us; 12us; 55us; 1us; 32768us; 12us; 54us; 0us; 16403us; 0us; 16404us; 2us; 16406us; 1us; 37us; 11us; 16us; 0us; 16405us; 1us; 32768us; 11us; 59us; 2us; 32768us; 1us; 66us; 2us; 65us; 1us; 32768us; 12us; 61us; 2us; 32768us; 1us; 37us; 11us; 16us; 1us; 32768us; 12us; 63us; 1us; 16408us; 11us; 58us; 0us; 16407us; 0us; 16409us; 1us; 16411us; 1us; 14us; 0us; 16410us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 2us; 3us; 5us; 6us; 8us; 11us; 13us; 15us; 16us; 18us; 20us; 22us; 24us; 25us; 27us; 28us; 36us; 39us; 41us; 43us; 44us; 46us; 48us; 49us; 52us; 54us; 55us; 58us; 60us; 61us; 64us; 66us; 67us; 70us; 72us; 73us; 74us; 75us; 77us; 79us; 81us; 83us; 86us; 88us; 91us; 93us; 94us; 97us; 99us; 100us; 103us; 104us; 108us; 110us; 111us; 112us; 115us; 116us; 118us; 121us; 123us; 126us; 128us; 130us; 131us; 132us; 134us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 5us; 4us; 5us; 2us; 1us; 5us; 5us; 4us; 4us; 4us; 4us; 1us; 1us; 10us; 4us; 2us; 1us; 5us; 4us; 2us; 1us; 7us; 6us; 1us; 2us; 1us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 3us; 4us; 4us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 6us; 6us; 7us; 7us; 8us; 8us; 9us; 9us; 10us; 10us; 10us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 65535us; 65535us; 65535us; 65535us; 16386us; 65535us; 65535us; 65535us; 65535us; 16388us; 65535us; 16389us; 65535us; 65535us; 65535us; 65535us; 16391us; 65535us; 65535us; 16392us; 65535us; 65535us; 16393us; 65535us; 65535us; 16394us; 65535us; 65535us; 16395us; 65535us; 65535us; 16396us; 16397us; 16398us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16399us; 65535us; 65535us; 16400us; 65535us; 16401us; 65535us; 65535us; 16403us; 16404us; 65535us; 16405us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16407us; 16409us; 65535us; 16410us; |]
let _fsyacc_reductions ()  =    [| 
# 176 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?>  fctSyntaxGrammar  in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : 'gentype__startparseProg));
# 185 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?>  fctSyntaxGrammar  in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "Lexing\Parser.fsp"
                                                  _1 
                   )
# 43 "Lexing\Parser.fsp"
                 :  fctSyntaxGrammar ));
# 196 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Column in
            let _2 = parseState.GetInput(2) :?>  fctNonterminal  in
            let _3 = parseState.GetInput(3) :?>  fctProduction list  in
            let _4 = parseState.GetInput(4) :?> Column in
            let _5 = parseState.GetInput(5) :?>  fctSyntaxGrammar  in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 49 "Lexing\Parser.fsp"
                                                                             (_2, _3) :: _5 
                   )
# 49 "Lexing\Parser.fsp"
                 :  fctSyntaxGrammar ));
# 211 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Column in
            let _2 = parseState.GetInput(2) :?>  fctNonterminal  in
            let _3 = parseState.GetInput(3) :?>  fctProduction list  in
            let _4 = parseState.GetInput(4) :?> Column in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 50 "Lexing\Parser.fsp"
                                                                   [(_2, _3)] 
                   )
# 50 "Lexing\Parser.fsp"
                 :  fctSyntaxGrammar ));
# 225 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Column in
            let _2 = parseState.GetInput(2) :?> Column in
            let _3 = parseState.GetInput(3) :?> string * Column in
            let _4 = parseState.GetInput(4) :?>  string list  in
            let _5 = parseState.GetInput(5) :?> Column in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 54 "Lexing\Parser.fsp"
                                                               (fst _3, _4) 
                   )
# 54 "Lexing\Parser.fsp"
                 :  fctNonterminal ));
# 240 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string * Column in
            let _2 = parseState.GetInput(2) :?>  string list  in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 58 "Lexing\Parser.fsp"
                                               [fst _1] @ _2 
                   )
# 58 "Lexing\Parser.fsp"
                 :  string list ));
# 252 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string * Column in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 59 "Lexing\Parser.fsp"
                                               [fst _1] 
                   )
# 59 "Lexing\Parser.fsp"
                 :  string list ));
# 263 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Column in
            let _2 = parseState.GetInput(2) :?> Column in
            let _3 = parseState.GetInput(3) :?> string * Column in
            let _4 = parseState.GetInput(4) :?>  fctProduction list  in
            let _5 = parseState.GetInput(5) :?> Column in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 64 "Lexing\Parser.fsp"
                                                                          
                                                                             List.map (fun prod ->  (FCase (fst _3))::prod ) _4
                                                                         
                   )
# 64 "Lexing\Parser.fsp"
                 :  fctProduction list ));
# 280 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Column in
            let _2 = parseState.GetInput(2) :?> Column in
            let _3 = parseState.GetInput(3) :?>  fctProduction list  in
            let _4 = parseState.GetInput(4) :?>  fctProduction list  in
            let _5 = parseState.GetInput(5) :?> Column in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 67 "Lexing\Parser.fsp"
                                                                          
                                                                             List.fold 
                                                                                 (fun acc prod ->  
                                                                                     (List.map (fun prod2 -> prod2@prod) _3)@acc
                                                                                 ) [] _4
                                                                         
                   )
# 67 "Lexing\Parser.fsp"
                 :  fctProduction list ));
# 300 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Column in
            let _2 = parseState.GetInput(2) :?> Column in
            let _3 = parseState.GetInput(3) :?>  (fctProduction list) * int  in
            let _4 = parseState.GetInput(4) :?> Column in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 73 "Lexing\Parser.fsp"
                                                                           fst _3 
                   )
# 73 "Lexing\Parser.fsp"
                 :  fctProduction list ));
# 314 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Column in
            let _2 = parseState.GetInput(2) :?> Column in
            let _3 = parseState.GetInput(3) :?>  fctProduction list  in
            let _4 = parseState.GetInput(4) :?> Column in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 74 "Lexing\Parser.fsp"
                                                                           List.map (fun prod ->  prod@[FDuplicate] ) _3 
                   )
# 74 "Lexing\Parser.fsp"
                 :  fctProduction list ));
# 328 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Column in
            let _2 = parseState.GetInput(2) :?> Column in
            let _3 = parseState.GetInput(3) :?>  fctProduction list  in
            let _4 = parseState.GetInput(4) :?> Column in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 75 "Lexing\Parser.fsp"
                                                                           List.map (fun prod ->  prod@[FEquality] ) _3 
                   )
# 75 "Lexing\Parser.fsp"
                 :  fctProduction list ));
# 342 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Column in
            let _2 = parseState.GetInput(2) :?> Column in
            let _3 = parseState.GetInput(3) :?> int * Column in
            let _4 = parseState.GetInput(4) :?> Column in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 76 "Lexing\Parser.fsp"
                                                                           [[FMakeNum (fst _3)]] 
                   )
# 76 "Lexing\Parser.fsp"
                 :  fctProduction list ));
# 356 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?>  fctProduction list  in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 77 "Lexing\Parser.fsp"
                                                                           _1 
                   )
# 77 "Lexing\Parser.fsp"
                 :  fctProduction list ));
# 367 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string * Column in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 78 "Lexing\Parser.fsp"
                                                                           [[FUseVar (fst _1)]]
                   )
# 78 "Lexing\Parser.fsp"
                 :  fctProduction list ));
# 378 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Column in
            let _2 = parseState.GetInput(2) :?> Column in
            let _3 = parseState.GetInput(3) :?> Column in
            let _4 = parseState.GetInput(4) :?> Column in
            let _5 = parseState.GetInput(5) :?>  string list  in
            let _6 = parseState.GetInput(6) :?> Column in
            let _7 = parseState.GetInput(7) :?>  fctProduction list  in
            let _8 = parseState.GetInput(8) :?> Column in
            let _9 = parseState.GetInput(9) :?>  fctProduction list  in
            let _10 = parseState.GetInput(10) :?> Column in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 79 "Lexing\Parser.fsp"
                                                                                                   
                                                                                                 List.fold 
                                                                                                     (fun acc prod ->  
                                                                                                         (List.map (fun prod2 -> prod@[FAddVar (_5)]@prod2) _9)@acc
                                                                                                     ) [] _7
                                                                                             
                   )
# 79 "Lexing\Parser.fsp"
                 :  fctProduction list ));
# 403 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Column in
            let _2 = parseState.GetInput(2) :?> string * Column in
            let _3 = parseState.GetInput(3) :?>  fctProduction list  in
            let _4 = parseState.GetInput(4) :?> Column in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 85 "Lexing\Parser.fsp"
                                                                           [[FNonterminal (fst _2, _3)]] 
                   )
# 85 "Lexing\Parser.fsp"
                 :  fctProduction list ));
# 417 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?>  fctProduction list  in
            let _2 = parseState.GetInput(2) :?>  fctProduction list  in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 90 "Lexing\Parser.fsp"
                                                           _1 @ _2 
                   )
# 90 "Lexing\Parser.fsp"
                 :  fctProduction list ));
# 429 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?>  fctProduction list  in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 91 "Lexing\Parser.fsp"
                                                           _1 
                   )
# 91 "Lexing\Parser.fsp"
                 :  fctProduction list ));
# 440 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Column in
            let _2 = parseState.GetInput(2) :?> Column in
            let _3 = parseState.GetInput(3) :?> string * Column in
            let _4 = parseState.GetInput(4) :?>  (fctProduction list) * int  in
            let _5 = parseState.GetInput(5) :?> Column in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 95 "Lexing\Parser.fsp"
                                                                   
                                                                     List.map (fun prod ->  prod@[FMake ((fst _3), (snd _4))] ) (fst _4)
                                                                 
                   )
# 95 "Lexing\Parser.fsp"
                 :  fctProduction list ));
# 457 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Column in
            let _2 = parseState.GetInput(2) :?> Column in
            let _3 = parseState.GetInput(3) :?> string * Column in
            let _4 = parseState.GetInput(4) :?> Column in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 98 "Lexing\Parser.fsp"
                                                                   [[FMake ((fst _3), 0)]] 
                   )
# 98 "Lexing\Parser.fsp"
                 :  fctProduction list ));
# 471 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?>  fctProduction list  in
            let _2 = parseState.GetInput(2) :?>  (fctProduction list) * int  in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 102 "Lexing\Parser.fsp"
                                                                   
                                                                     (List.fold 
                                                                         (fun acc prod ->  
                                                                             (List.map (fun prod2 -> prod@prod2) _1)@acc
                                                                         ) [] (fst _2)
                                                                     , (snd _2) + 1)
                                                                 
                   )
# 102 "Lexing\Parser.fsp"
                 :  (fctProduction list) * int ));
# 489 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?>  fctProduction list  in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 109 "Lexing\Parser.fsp"
                                                                   (_1, 1) 
                   )
# 109 "Lexing\Parser.fsp"
                 :  (fctProduction list) * int ));
# 500 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Column in
            let _2 = parseState.GetInput(2) :?> Column in
            let _3 = parseState.GetInput(3) :?>  fctSyntaxTerminal  in
            let _4 = parseState.GetInput(4) :?> Column in
            let _5 = parseState.GetInput(5) :?>  fctProduction list  in
            let _6 = parseState.GetInput(6) :?> Column in
            let _7 = parseState.GetInput(7) :?>  fctProduction list  in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 114 "Lexing\Parser.fsp"
                                                                           
                                                                             (List.map (fun prod ->  _3::prod ) _5) @ _7 
                                                                         
                   )
# 114 "Lexing\Parser.fsp"
                 :  fctProduction list ));
# 519 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Column in
            let _2 = parseState.GetInput(2) :?> Column in
            let _3 = parseState.GetInput(3) :?>  fctSyntaxTerminal  in
            let _4 = parseState.GetInput(4) :?> Column in
            let _5 = parseState.GetInput(5) :?>  fctProduction list  in
            let _6 = parseState.GetInput(6) :?> Column in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 117 "Lexing\Parser.fsp"
                                                                           
                                                                             List.map (fun prod ->  _3::prod ) _5 
                                                                         
                   )
# 117 "Lexing\Parser.fsp"
                 :  fctProduction list ));
# 537 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> int * Column in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 123 "Lexing\Parser.fsp"
                                           FCaseNum (fst _1) 
                   )
# 123 "Lexing\Parser.fsp"
                 :  fctSyntaxTerminal ));
# 548 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string * Column in
            let _2 = parseState.GetInput(2) :?>  string list  in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 124 "Lexing\Parser.fsp"
                                           FCaseVar ((fst _1), _2) 
                   )
# 124 "Lexing\Parser.fsp"
                 :  fctSyntaxTerminal ));
# 560 "bin\Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string * Column in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 125 "Lexing\Parser.fsp"
                                           FCaseVar ((fst _1), []) 
                   )
# 125 "Lexing\Parser.fsp"
                 :  fctSyntaxTerminal ));
|]
# 572 "bin\Parser.fs"
let tables : FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 16;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = tables.Interpret(lexer, lexbuf, startState)
let parseProg lexer lexbuf :  fctSyntaxGrammar  =
    engine lexer lexbuf 0 :?> _
