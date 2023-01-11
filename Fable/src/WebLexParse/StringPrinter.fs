module StringPrinter

open AbSyn


let StringPrintGrammar(grammar : GrammarProgram) : string=
    let mutable res = ""
    for (nt, prods) in grammar do
        res <- res + sprintf "%A:\n" nt
        for prod in prods do
            res <- res + sprintf "["
            for term in prod do
                match term with
                | Permute order -> 
                    res <- res + sprintf "Permute ("
                    for i in order do 
                        res <- res + sprintf "%i " i
                    res <- res + sprintf "); "
                | x ->
                    res <- res + sprintf "%A; " x
            res <- res + sprintf "]\n"
    res


let uncompressedStringPrint(fs, gs, hs) : string =
    let mutable res = ""
    for fprod in fs do
        res <- res + sprintf "%A\n" fprod
    res <- res + "\n"
    for gprod in gs do
        res <- res + sprintf "%A\n" gprod
    res <- res + "\n"
    for hprod in hs do
        res <- res + sprintf "%A\n" hprod
    res