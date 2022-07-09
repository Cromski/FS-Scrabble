module internal Parser

    open Eval
    open ScrabbleUtil
    open FParsecLight.TextParser
    open StateMonad
    
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

    let pif         = pstring "if"
    let pthen       = pstring "then"
    let pelse       = pstring "else"
    let pwhile      = pstring "while"
    let pdo         = pstring "do"
    let pdeclare    = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace
    let pletter        = satisfy System.Char.IsLetter 
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit

    let spaces          = many whitespaceChar
    let spaces1        =  many1 whitespaceChar

    let (.>*>.) p1 p2 = (p1 .>> spaces) .>>. p2
    let (.>*>) p1 p2  = (p1 .>> spaces) .>> p2
    let (>*>.) p1 p2  = (p1 .>> spaces) >>. p2

    let parenthesise p = pstring "(" >*>. p .>*> pstring ")"
    let tuborgParenthesise p = pstring "{" >*>. p .>*> pstring "}"

    let pid = pchar '_' <|> pletter .>>. many (palphanumeric <|> pchar '_') |>> (fun (x, y) -> List.fold (fun acc z -> acc + (string z)) (string x) y)

    
    let unop p1 p2 = p1 >*>. p2 
    let binop op p1 p2 = (p1 .>*> op) .>*>. p2 

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    
    let DivParse = binop (pchar '/') AtomParse TermParse |>> Div <?> "Div"
    
    
    let MulParse = binop (pchar '*') AtomParse TermParse |>> Mul <?> "Mul"
    
    let ModParse = binop (pchar '%') AtomParse TermParse |>> Mod <?> "Mod"

    do tref.Value <- choice [AddParse; SubParse; ProdParse]
    do pref.Value <- choice [MulParse; DivParse; ModParse; AtomParse]
    
    
    let NParse   = pint32 |>> N <?> "Int"
    
    let NegParse = pchar '-' >>. NParse |>> (fun y ->  Mul(N -1, y)) <?> "Neg"
    let VarParse = pid |>> V <?> "var"
    let ParParse = parenthesise TermParse
    let PVParse = pPointValue >*>. ParParse |>> PV
    
    let CharParse, cref = createParserForwardedToRef<cExp>() 
    let ParCharParse = parenthesise CharParse

    let toUpper = pToUpper >*>. ParCharParse |>> ToUpper <?> "c ToUpper"
    let toLower = pToLower >*>. ParCharParse |>> ToLower <?> "c ToLower"
    let intToChar = pIntToChar >*>. ParParse |>> IntToChar <?> "c IntToChar"
    let charToInt = pCharToInt >*>. ParCharParse |>> CharToInt <?> "c CharToInt"
    let CV = pCharValue >*>. ParParse |>> CV <?> "c CV"
    let C  = pchar ''' >>. anyChar .>> pchar ''' |>> C <?> "c C"
    
    do aref.Value <- choice [NegParse; charToInt; PVParse; NParse; VarParse; ParParse]

    do cref.Value <- choice [CV; intToChar; toUpper; toLower; C; ParCharParse]

    let AexpParse = TermParse 

    let CexpParse = CharParse

    

    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"

(* These five types will move out of this file once you start working on the project *)

    type word   = (char * int) list
    
    type square = Map<int, squareFun>

    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"

    type squareFun = word -> int -> int -> Result<int, Error>
    type boardFun2 = coord -> StateMonad.Result<square option, Error>
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }

    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}

