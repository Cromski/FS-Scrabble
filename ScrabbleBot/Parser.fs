// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open System
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    open Eval

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open FParsecLight.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"
    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy (fun x -> System.Char.IsWhiteSpace x) <?> "whitespace"
    let pletter        = satisfy (fun x -> System.Char.IsLetter x) <?> "letter"
    let palphanumeric  = satisfy (fun x -> System.Char.IsLetterOrDigit x) <?> "alphanumeric"

    let spaces         = many (whitespaceChar) <?> "space"
    let spaces1        = many1 (whitespaceChar) <?> "space1"

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2 
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 .>> spaces >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'  // incorrect (not implemented)

    let pid = pchar '_' <|> pletter .>>. many (palphanumeric <|> pchar '_') |>> (
        fun (x, y) -> List.fold (
            fun acc z -> acc + (string z)) (string x) y)           
    let unop op a = op >*>. a
    let binop op p1 p2 = (p1 .>*> op) .>*>. p2 // incorrect (not implemented)

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"

    
    let DivParse = binop (pchar '/') AtomParse TermParse |>> Div <?> "Div"
    
    
    let MulParse = binop (pchar '*') AtomParse TermParse |>> Mul <?> "Mul"
    
    let ModParse = binop (pchar '%') AtomParse TermParse |>> Mod <?> "Mod"
 
    
   
    
    let NParse   = pint32 |>> N <?> "Int"
    
    let NegParse = pchar '-' >>. NParse |>> (fun y ->  Mul(N -1, y)) <?> "Neg"
    let VarParse = pid |>> V <?> "var"
    let ParParse = parenthesise TermParse
    let PVParse = pPointValue >*>. ParParse |>> PV
    

    let AexpParse = TermParse 

    let CTermParse, cref = createParserForwardedToRef<cExp>()
    
    let CarParse = parenthesise CTermParse
    
    let anyC = pchar ''' >>. anyChar .>> pchar ''' |>> C <?> "any C"
    let cValue = pCharValue >*>. ParParse |>> CV <?> "cValue"
    
    let intToChar = pIntToChar >*>. ParParse |>> IntToChar <?> "integer to char"
    let PtoUpper = pToUpper >*>. CarParse |>> ToUpper <?> "ToUpper C"
    let PtoLower = pToLower >*>. CarParse |>> ToLower <?> "CharToLower"
    let charToInt = pCharToInt >*>. CarParse |>> CharToInt <?> "char to integer"
    
    do tref := choice [AddParse; SubParse; ProdParse]
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]
    do aref := choice [charToInt;  NegParse; PVParse; NParse; VarParse; ParParse]
    do cref := choice [cValue; intToChar; PtoUpper; PtoLower; anyC; CarParse]
    let CexpParse = CTermParse
    
    let BexpParse = pstring "not implemented"
    let stmntParse = pstring "not implemented"

(* These five types will move out of this file once you start working on the project *)
    type word   = (char * int) list
    type square = Map<int, squareFun>

    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"

    type squareFun = word -> int -> int -> Result<int, Error>
    type boardFun2 = coord -> StateMonad.Result<square option, StateMonad.Error>
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
