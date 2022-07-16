namespace OsmanOgJeppe

open Eval
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 


    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board                 : Parser.board
        dict                  : Dictionary.Dict
        playerNumber          : uint32
        mutable hand          : MultiSet.MultiSet<uint32>
        mutable points        : uint32
        mutable coordsOfChars         : Map<coord, char>
    }
(*
    type brick = char*int
    type id2brick = uint32*brick
    type play = (coord * id2brick) list *)

    type tile = char * int
    type piece = uint32 * tile
    type move = (coord * piece) list
    type word = (coord * (uint32 * (char * int))) list


    let mkState b d pn h p map = {board = b; dict = d;  playerNumber = pn; hand = h; points = p; coordsOfChars = map}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let boardOverview st = st.coordsOfChars

module Scrabble =
    open System.Threading
    

    //Vores heuristic, som tager 2 ord og løber gennem hvert ord, tager summen af ordene og returnere det ord med flest point.
    let highestPointWord (word1: State.word) (word2: State.word) : State.word =
        let pointsWord1 = List.fold (fun acc word' -> (word' |> snd |> snd |> snd) |> (+) acc) 0 word1
        let pointsWord2 = List.fold (fun acc word' -> (word' |> snd |> snd |> snd) |> (+) acc) 0 word2

        match pointsWord1 with
        | x when x >= pointsWord2 -> word1
        | _ -> word2


    //Finder det tile på boardet der enten ikke har noget til venstre for den eller ovenover den, alt efter hvilken værdig xWordBool har.
    //Returnere et koordinat til en tile.
    let rec startTileOfWord coordinates xWordBool board =
        
        let (x_coordinate, y_coordinate) = coordinates
        let newCoords =
            match xWordBool with
            | true -> (x_coordinate - 1, y_coordinate)
            | _ -> (x_coordinate, y_coordinate - 1)

        match Map.tryFind newCoords board with
        | None -> coordinates
        | Some _ -> startTileOfWord newCoords xWordBool board

        
    //Takes current coordinates and returns a new set of coords.
    //The new coords returned, depends on xWordBool and Subtract.
    let changeCoords xWordBool subtract (x_coordinate, y_coordinate)=
        match xWordBool with
        | true -> if subtract then (x_coordinate - 1, y_coordinate) else (x_coordinate + 1, y_coordinate)
        | _ -> if subtract then (x_coordinate, y_coordinate - 1) else (x_coordinate, y_coordinate + 1)


    //Checks if the current word that is being played, can be played legally.
    //Returns false if there is any adjacent tiles, to the word you are trying to play.
    let checkIfLegal coordinates coordsOfChars =
        //højre, venstre
        let horizontal = [changeCoords true true coordinates; changeCoords true false coordinates]
        //op, ned
        let vertical = [changeCoords false true coordinates; changeCoords false false coordinates]
        
        //højre, venstre, op, ned
        [Map.containsKey horizontal[0] coordsOfChars; Map.containsKey horizontal.[1] coordsOfChars; 
         Map.containsKey vertical.[0] coordsOfChars; Map.containsKey vertical.[1] coordsOfChars] 

        
    let finalWord word endOfWord highestPointValueWord placePiece accumulator=
                                match word with
                                | None ->
                                    if endOfWord
                                        then
                                        highestPointWord highestPointValueWord placePiece
                                    else
                                        accumulator
                                | Some (_) -> accumulator

    let placePiece coords pieceId character value newWord =
                                                        ((coords, (pieceId, (character, value)))
                                                        :: newWord)

    let newState state dict' pieceId coords character =
                        { state with
                            State.dict = dict'
                            State.hand = MultiSet.removeSingle pieceId state.hand
                            State.coordsOfChars = Map.add coords character state.coordsOfChars }
    
    let rec findMove xWordBool coords (state: State.state) pieces newWord highestPointValueWord
        : State.word =
        let x_coordinate, y_coordinate = coords

        //finds new coordinates by incrementing x and y value with 1 each time
        let newCoordinates =
            changeCoords xWordBool false
                (x_coordinate, y_coordinate)

        (*
        Looks to see if there is a char on the board at the given coords.
        If it matches with "|Some" we've found a char, otherwise, if it matches with "|None", 
        we didn't find a char at the given coords
        *)
        match Map.tryFind coords state.coordsOfChars with
        | Some characterOnBoard ->
            match Dictionary.step characterOnBoard state.dict with
            | Some (endOfWord, dict') ->
                let stateUpdated = { state with dict = dict' }

                findMove
                    xWordBool
                    newCoordinates
                    stateUpdated
                    pieces
                    newWord
                    (if endOfWord
                        && (Map.containsKey newCoordinates state.coordsOfChars)
                           |> not then
                         highestPointWord newWord highestPointValueWord
                     else
                         highestPointValueWord)
            | None -> highestPointValueWord
        | None ->
            MultiSet.fold
                (fun accumulator pieceId _ ->
                    highestPointWord
                        (Set.fold
                            (fun accumulator (character, value) ->
                                match Dictionary.step character state.dict with
                                | None -> accumulator
                                | Some (endOfWord, dict') ->
                                    let checklegal = checkIfLegal coords state.coordsOfChars
                                    match xWordBool with
                                    //dict' pieceId state coords character value newWord newCoordinates endOfWord highestPointValueWord placePiece 
                                    | true -> if not checklegal[2] && not checklegal[3] then

                                                let word =
                                                    Map.tryFind newCoordinates state.coordsOfChars

                                                let placePiece = placePiece coords pieceId character value newWord

                                                findMove
                                                    xWordBool
                                                    newCoordinates
                                                    (newState state dict' pieceId coords character)
                                                    pieces
                                                    placePiece 
                                                    (finalWord word endOfWord highestPointValueWord placePiece accumulator)
                                                else
                                                    accumulator
                                    | false -> if not checklegal[0] && not checklegal[1] then
                                                let word =
                                                    Map.tryFind newCoordinates state.coordsOfChars

                                                let placePiece = placePiece coords pieceId character value newWord

                                                findMove
                                                    xWordBool
                                                    newCoordinates
                                                    (newState state dict' pieceId coords character)
                                                    pieces
                                                    placePiece 
                                                    (finalWord word endOfWord highestPointValueWord placePiece accumulator)
                                                else
                                                    accumulator
                                                    )
                                    
                            highestPointValueWord
                            (Map.find pieceId pieces))
                        accumulator)
                highestPointValueWord
                state.hand

    
                
                
    let botMove (piecesOnBoard: Map<coord, char>) (state: State.state) (pieces: Map<uint32, Set<char * int>>) : State.word =
            let list = (Seq.toList (Map.keys state.coordsOfChars))

            if Map.count piecesOnBoard = 0 then
               findMove true (0,0) state pieces [] []
            else
                let rec checkCoords coordinates : State.word =
                    match coordinates with
                    | [] -> []
                    | (xC, yC) :: xs ->
                        findMove true (startTileOfWord (xC, yC) true state.coordsOfChars) state pieces [] []
                        |> highestPointWord (
                            findMove false (startTileOfWord (xC, yC) false state.coordsOfChars) state pieces [] []
                        )
                        |> highestPointWord (checkCoords xs)

                checkCoords list
    
    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            let move = botMove st.coordsOfChars st pieces
            
            (*let input =  System.Console.ReadLine()
            let move = RegEx.parseMove input*)

            match move with
            | x when x <> List.Empty -> send cstream (SMPlay move)
            | _ -> send cstream SMPass
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                
                //removes old pieces from hand
                for play in ms do st.hand <- MultiSet.removeSingle (play |> snd |> fst) st.hand
                
                //adds new pieces to hand
                for piece in newPieces do st.hand <- MultiSet.add (fst piece) (snd piece) st.hand
                
                //add points
                st.points <- st.points + uint32 points
                
                for piece in ms do st.coordsOfChars <- Map.add (fst piece) (piece |> snd |> snd |> fst) st.coordsOfChars 
                                
                let st' = st 

                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = st
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                //
                
                
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st



        aux st


    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            $"Starting game!
                      number of players = %d{numPlayers}
                      player id = %d{playerNumber}
                      player turn = %d{playerTurn}
                      hand =  %A{hand}
                      timeout = %A{timeout}\n\n"

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet 0u Map.empty)