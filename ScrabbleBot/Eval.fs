// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval
    
    open System
    open StateMonad

    (* Code for testing *)

    let hello = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1);]
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    // sm<res> >>= fun res -> res2 
    let add a b =
        a >>= (fun x -> b >>= fun y -> ret(x + y))
     
    let sub a b =
        a >>= (fun x -> b >>= fun y -> ret(x - y))
        
    let mul a b =
        a >>= (fun x -> b >>= fun y -> ret (x * y))
    let div a b =
        a >>= (fun x -> b >>= fun y -> if y <> 0 then ret (x / y) else fail (DivisionByZero))
    
    let modulo a b =
        a >>= (fun x -> b >>= fun y -> if y <> 0 then ret (x % y) else fail (DivisionByZero))    

    let charToInt c = c >>= fun x -> ret (x - int '0')
    
    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval a : SM<int> =
        match a with
         | N n -> ret n
         | V v -> lookup v
         | WL -> wordLength
         | PV x -> arithEval x >>= pointValue
         | Add (b, c)-> add (arithEval b) (arithEval c)
         | Sub (b, c) -> sub (arithEval b) (arithEval c)
         | Mul (b, c) -> mul (arithEval b) (arithEval c)
         | Div (a, b) -> div (arithEval a) (arithEval b)
         | Mod (a, b) -> modulo (arithEval a) (arithEval b)
         | CharToInt a -> (charEval a) >>= fun x -> ret (int x) 

    and charEval c : SM<char> =
        match c with
         | C a -> ret a  (* Character value *)
         | CV exp-> (arithEval exp) >>= characterValue (* Character lookup at word index *)
         | ToUpper a -> charEval a >>= fun x -> ret (System.Char.ToUpper x) 
         | ToLower a -> charEval a >>= fun x -> ret (System.Char.ToLower x) 
         | IntToChar a -> (arithEval a) >>= fun x -> ret (char x)

    let vowel:char list = ['a'; 'e'; 'i'; 'o'; 'u'; 'y'; 'A';'E';'I';'O';'U';'Y']
    let rec boolEval b : SM<bool> =
        match b with
         | TT -> ret true               (* true *)
         | FF  -> ret false                 (* false *)
         | AEq (c, d) -> (arithEval c) >>= fun x -> (arithEval d) >>= fun y -> ret (x = y)  (* numeric equality *)
         | ALt (c, d) ->
             (arithEval c) >>= fun x -> (arithEval d) >>= fun y -> ret (x < y)
         | Not c -> (boolEval c) >>= fun x -> ret (not x)       (* boolean not *)
         | Conj (c, d) -> (boolEval c) >>= fun x -> (boolEval d) >>= fun y -> ret (x && y)
         | IsDigit c -> (charEval c) >>= fun x -> ret(Char.IsDigit (x))
         | IsLetter c -> (charEval c) >>= fun x -> ret(Char.IsLetter(x))   (* check for vowel *)
         | IsVowel c -> (charEval c) >>= fun x -> ret(List.contains x vowel)



    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = failwith "Not implemented"


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
    