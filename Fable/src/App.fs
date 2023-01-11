module App

open Browser.Dom


let invertButton = document.querySelector(".AbsCenter") :?> Browser.Types.HTMLButtonElement
let interpretButton = document.querySelector("#interpret3") :?> Browser.Types.HTMLButtonElement

let uncompressedHTML = document.querySelector("#uncompressedGrammar") :?> Browser.Types.HTMLElement
let invertedHTML = document.querySelector("#invertedGrammar") :?> Browser.Types.HTMLElement

let outStackHTML = document.querySelector("#interpret2") :?> Browser.Types.HTMLElement

invertButton.onclick <- fun _ ->
    let inpGrammarHTML = document.querySelector("#grammarText") :?> Browser.Types.HTMLTextAreaElement
    let grammarString = inpGrammarHTML.value    
    let grammar = GrammarLex.GrammarLexer(grammarString)
    let invGram, (fs, gs, hs) = GrammarLex.InvertGrammar(grammar)
    let strink = StringPrinter.StringPrintGrammar(invGram)
    invertedHTML.innerText <- strink
    let uncomString = StringPrinter.uncompressedStringPrint(fs, gs, hs)
    uncompressedHTML.innerText <- uncomString


interpretButton.onclick <- fun _ ->
    let inpGrammarHTML = document.querySelector("#grammarText") :?> Browser.Types.HTMLTextAreaElement
    let grammarString = inpGrammarHTML.value    
    let grammar = GrammarLex.GrammarLexer(grammarString)

    let inpStackHTML = document.querySelector("#interpret1") :?> Browser.Types.HTMLTextAreaElement
    let stackString = inpStackHTML.value
    let parsedStack = StackLex.LexStack(stackString)
    
    let stackOut = Interpreter.PrettyPrintStack(grammar, parsedStack)
    outStackHTML.innerText <- (sprintf "%s" stackOut)


//Fable does not support System.IO, 
// so the grammar examples are written as strings




let inc = document.querySelector(".inc") :?> Browser.Types.HTMLButtonElement
let snoc = document.querySelector(".snoc") :?> Browser.Types.HTMLButtonElement
let rps = document.querySelector(".rps") :?> Browser.Types.HTMLButtonElement


inc.onclick <- fun _ ->
    let incString = "\"inc\":
  [CaseNum 1; MakeNum 1; MakeNum 0; Make (\"cons\", 2)]
  [Case \"cons\"; CaseNum 0; MakeNum 1; Make (\"cons\", 2)]
  [Case \"cons\"; CaseNum 1; Nonterminal \"inc\"; MakeNum 0; Make (\"cons\", 2)]"

    let inpGrammarHTML = document.querySelector("#grammarText") :?> Browser.Types.HTMLTextAreaElement
    inpGrammarHTML.value <- incString

snoc.onclick <- fun _ ->
    let snocString = "\"snoc\":
  [Case \"nil\"; Make (\"nil\", 0); Permute (2, 1); Make (\"cons\", 2)]
  [Case \"cons\"; Permute (2, 3, 1); Nonterminal \"snoc\"; Permute (2, 1); Make (\"cons\", 2)]"
    let snocGrammarHTML = document.querySelector("#grammarText") :?> Browser.Types.HTMLTextAreaElement
    snocGrammarHTML.value <- snocString

rps.onclick <- fun _ -> 
    let rpsString = 
        "\"RPS\": 
    [CaseEquality; Case \"RPS\"; Nonterminal \"RemoveSymbol\"; MakeNum 0];
    [CaseNonEquality; Case \"RPS\"; Nonterminal \"ResolveSigns\"]
\"ResolveSigns\":
    [Case \"Rock\"; Case \"Scissor\"; MakeNum 1];
    [Case \"Rock\"; Case \"Paper\"; MakeNum -1];
    [Case \"Scissor\"; Case \"Rock\"; MakeNum -1];
    [Case \"Scissor\"; Case \"Paper\"; MakeNum 1];
    [Case \"Paper\"; Case \"Rock\"; MakeNum 1];
    [Case \"Paper\"; Case \"Scissor\"; MakeNum -1]
\"RemoveSymbol\":
    [Case \"Rock\"];
    [Case \"Scissor\"];
    [Case \"Paper\"]
\"NeverRun\":
    [Make (\"RPS\", 2); Make (\"Rock\", 0); Make (\"Paper\", 0); Make (\"Scissor\", 0)]"
    let rpsGrammarHTML = document.querySelector("#grammarText") :?> Browser.Types.HTMLTextAreaElement
    rpsGrammarHTML.value <- rpsString
