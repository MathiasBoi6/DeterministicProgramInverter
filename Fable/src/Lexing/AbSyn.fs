module  AbSyn


(*--- Grammar types ---*)

type GrammarTerminal =
    | Case of string
    | Make of string * int
    | MakeNum of int
    | CaseNum of int
    | Nonterminal of string
    | Permute of int list
    | Duplicate
    | CaseEquality
    | CaseNonEquality

type Production = GrammarTerminal list

type Nonterminal = string * Production list

type GrammarProgram = Nonterminal list


(*--- Stack types ---*)

type Value =
    | NUM of int
    | CONS of string * (Value list)

(*--- Lexer/Parser types ---*)

type Column = int

type fctNonterminal = string * (string list) 

type fctSyntaxTerminal =
    | FMake of string * int
    | FCaseVar of string * (string list)
    | FCase of string
    | FMakeNum of int
    | FCaseNum of int
    | FNonterminal of string * (fctSyntaxTerminal list list)
    | FUseVar of string
    | FAddVar of string list
    | FDuplicate
    | FEquality
    | FRemoveTop

type fctProduction = fctSyntaxTerminal list

type fctNonterminalProds = fctNonterminal * (fctProduction list)

type fctSyntaxGrammar = fctNonterminalProds list


(*--- NFA types ---*)

type NFATerminal =
    | Terminal of GrammarTerminal
    | Epsilon of string