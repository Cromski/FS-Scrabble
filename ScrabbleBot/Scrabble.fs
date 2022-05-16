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
        piecesOnBoard         : Map<coord, char>
    }
(*
    type brick = char*int
    type id2brick = uint32*brick
    type play = (coord * id2brick) list *)

    type tile = char * int
    type piece = uint32 * tile
    type move = (coord * piece) list
    type word = (coord * (uint32 * (char * int))) list


    let mkState b d pn h p map = {board = b; dict = d;  playerNumber = pn; hand = h; points = p; piecesOnBoard = map}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let piecesOnBoard st = st.piecesOnBoard

module Scrabble =
    open System.Threading
    
    let highestPointWord (word1: State.word) (word2: State.word) : State.word =
        let pointsWord1 = List.fold (fun acc word' -> (word' |> snd |> snd |> snd) |> (+) acc) 0 word1
        let pointsWord2 = List.fold (fun acc word' -> (word' |> snd |> snd |> snd) |> (+) acc) 0 word2

        match pointsWord1 with
        | x when x >= pointsWord2 -> word1
        | _ -> word2

    let getCoords isHorizontal (xCoord, yCoord) : coord =
        if isHorizontal then
            (xCoord + 1, yCoord)
        else
            (xCoord, yCoord + 1)

    let rec specifiedStartingCoordinates coord isHorizontal board =
        let xCoord, yCoord = coord

        let changedCoords =
            if isHorizontal then
                (xCoord - 1, yCoord)
            else
                (xCoord, yCoord - 1)

        match Map.tryFind changedCoords board with
        | None -> coord
        | Some _ -> specifiedStartingCoordinates changedCoords isHorizontal board

    let isLegalMove (isHorizontal: bool) (coord: coord) (piecesOnBoard: Map<coord, char>) =
        let (xCoord, yCoord) = coord

        let minusXY =
            if isHorizontal then
                (xCoord, yCoord - 1)
            else
                (xCoord - 1, yCoord)

        let plusXY =
            if isHorizontal then
                (xCoord, yCoord + 1)
            else
                (xCoord + 1, yCoord)

        if (not (
                (Map.containsKey minusXY piecesOnBoard)
                || (Map.containsKey plusXY piecesOnBoard)
            )) then
            true
        else
            false

    let rec findMove
        (isHorizontal: bool)
        (coordinates: coord)
        (st: State.state)
        (pieces: Map<uint32, Set<char * int>>)
        (piecesLaidOnTable: State.word)
        (longestWord: State.word)
        : State.word =
        let xCoords, yCoords = coordinates

        let changedCoords =
            getCoords isHorizontal (xCoords, yCoords)

        match Map.tryFind coordinates st.piecesOnBoard with
        | Some (characterOnBoard) ->
            match Dictionary.step characterOnBoard st.dict with
            | Some (wordEnded, trie) ->
                let stateUpdated = { st with dict = trie }

                findMove
                    isHorizontal
                    changedCoords
                    stateUpdated
                    pieces
                    piecesLaidOnTable
                    (if wordEnded
                        && (Map.containsKey changedCoords st.piecesOnBoard)
                           |> not then
                        highestPointWord longestWord piecesLaidOnTable
                     else
                        longestWord)
            | None -> longestWord
        | None ->
            MultiSet.fold
                (fun accumulator pieceId _ ->
                    highestPointWord
                        (Set.fold
                            (fun accumulator (charPiece, pointValue) ->
                                match Dictionary.step charPiece st.dict with
                                | None -> accumulator
                                | Some (endW, trie) ->
                                    if isLegalMove isHorizontal coordinates st.piecesOnBoard then
                                        let stateUpdated =
                                            { st with
                                                  dict = trie
                                                  hand = MultiSet.removeSingle pieceId st.hand
                                                  piecesOnBoard = Map.add coordinates charPiece st.piecesOnBoard }

                                        let piecesDownOnBoard =
                                            ((coordinates, (pieceId, (charPiece, pointValue)))
                                             :: piecesLaidOnTable)

                                        let bestPossibleWord =
                                            Map.tryFind changedCoords st.piecesOnBoard

                                        let resultBestPossibleWord =
                                            match bestPossibleWord with
                                            | None ->
                                                if endW
                                                   && (Map.containsKey changedCoords st.piecesOnBoard)
                                                      |> not then
                                                    highestPointWord longestWord piecesDownOnBoard
                                                else
                                                    accumulator
                                            | Some (_) -> accumulator

                                        findMove
                                            isHorizontal
                                            changedCoords
                                            stateUpdated
                                            pieces
                                            piecesDownOnBoard
                                            resultBestPossibleWord
                                    else
                                        accumulator)
                            longestWord
                            (Map.find pieceId pieces))
                        accumulator)
                longestWord
                st.hand

    let botMove (piecesOnBoard: Map<coord, char>) (st: State.state) (pieces: Map<uint32, Set<char * int>>) : State.word =
        let coordsToList = (Seq.toList (Map.keys st.piecesOnBoard))

        if Map.count piecesOnBoard = 0 then
            highestPointWord (findMove true (0, 0) st pieces [] []) (findMove false (0, 0) st pieces [] [])
        else
            let rec runThroughCoords coordinates : State.word =
                match coordinates with
                | [] -> []
                | (xC, yC) :: xs ->
                    findMove true (specifiedStartingCoordinates (xC, yC) true st.piecesOnBoard) st pieces [] []
                    |> highestPointWord (
                        findMove false (specifiedStartingCoordinates (xC, yC) false st.piecesOnBoard) st pieces [] []
                    )
                    |> highestPointWord (runThroughCoords xs)

            runThroughCoords coordsToList

    // let rec startTileOfWord coordinates xWordBool board =
        
    //     let (x_coordinate, y_coordinate) = coordinates
    //     let newCoords =
    //         match xWordBool with
    //         | true -> (x_coordinate - 1, y_coordinate)
    //         | _ -> (x_coordinate, y_coordinate - 1)

    //     match Map.tryFind newCoords board with
    //     | None -> coordinates
    //     | Some _ -> startTileOfWord newCoords xWordBool board

    // let changeCoords xWordBool subtract (x_coordinate, y_coordinate)=
    //     match xWordBool with
    //     | true -> if subtract then (x_coordinate - 1, y_coordinate) else (x_coordinate + 1, y_coordinate)
    //     | _ -> if subtract then (x_coordinate, y_coordinate - 1) else (x_coordinate, y_coordinate + 1)

    // let checkIfLegal xWordBool coordinates coordsOfChars =
    //     //TODO: check this if error occurs
    //     let subXorYcoord = changeCoords (not xWordBool) true coordinates
    //     let addXorYcoord = changeCoords (not xWordBool) false coordinates

    //     match ((Map.containsKey subXorYcoord coordsOfChars) || (Map.containsKey addXorYcoord coordsOfChars)) with
    //     | false -> true
    //     | _ -> false
    
    
    // let rec findMove xWordBool coords (state: State.state) pieces newWord highestPointValueWord
    //     : State.word =
    //     let x_coordinate, y_coordinate = coords

    //     let newCoordinates =
    //         changeCoords xWordBool false (x_coordinate, y_coordinate)

    //     match Map.tryFind coords state.coordsOfChars with
    //     | Some (characterOnBoard) ->
    //         match Dictionary.step characterOnBoard state.dict with
    //         | Some (endOfWord, dict') ->
    //             let stateUpdated = { state with dict = dict' }

    //             findMove
    //                 xWordBool
    //                 newCoordinates
    //                 stateUpdated
    //                 pieces
    //                 newWord
    //                 (if endOfWord && not (Map.containsKey newCoordinates state.coordsOfChars)
    //                 then
    //                     highestPointWord newWord highestPointValueWord
    //                 else
    //                     highestPointValueWord)
    //         | None -> highestPointValueWord
    //     | None ->
    //         MultiSet.fold
    //             (fun accumulator pieceId _ ->
    //                 highestPointWord
    //                     (Set.fold
    //                         (fun accumulator (character, value) ->
    //                             match Dictionary.step character state.dict with
    //                             | None -> accumulator
    //                             | Some (endOfWord, dict') ->
    //                                 if checkIfLegal xWordBool coords state.coordsOfChars then
    //                                     let newState =
    //                                         { state with
    //                                               dict = dict'
    //                                               hand = MultiSet.removeSingle pieceId state.hand
    //                                               coordsOfChars = Map.add coords character state.coordsOfChars }

    //                                     let placePiece =
    //                                         ((coords, (pieceId, (character, value)))
    //                                          :: newWord)

    //                                     let word =
    //                                         Map.tryFind newCoordinates state.coordsOfChars

    //                                     let finalWord =
    //                                         match word with
    //                                         | None ->
    //                                             if endOfWord && not (Map.containsKey newCoordinates state.coordsOfChars)
    //                                             then
    //                                                 highestPointWord highestPointValueWord placePiece
    //                                             else
    //                                                 accumulator
    //                                         | Some (_) -> accumulator

    //                                     findMove
    //                                         xWordBool
    //                                         newCoordinates
    //                                         newState
    //                                         pieces
    //                                         placePiece
    //                                         finalWord
    //                                 else
    //                                     accumulator)
    //                         highestPointValueWord
    //                         (Map.find pieceId pieces))
    //                     accumulator)
    //             highestPointValueWord
    //             state.hand
                
                
    // let botMove (piecesOnBoard: Map<coord, char>) (state: State.state) (pieces: Map<uint32, Set<char * int>>) : State.word =
    //         let list = (Seq.toList (Map.keys state.coordsOfChars))

    //         if Map.count piecesOnBoard = 0 then
    //            findMove false (0,0) state pieces [] []
    //         else
    //             let rec checkCoords coordinates : State.word =
    //                 match coordinates with
    //                 | [] -> []
    //                 | (xC, yC) :: xs ->
    //                     findMove true (startTileOfWord (xC, yC) true state.coordsOfChars) state pieces [] []
    //                     |> highestPointWord (
    //                         findMove false (startTileOfWord (xC, yC) false state.coordsOfChars) state pieces [] []
    //                     )
    //                     |> highestPointWord (checkCoords xs)

    //             checkCoords list
    
    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            let move = botMove st.piecesOnBoard st pieces
            
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
                
                debugPrint (sprintf "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\n%A\n" newPieces)
                //removes old pieces from hand
                for play in ms do st.hand <- MultiSet.removeSingle (play |> snd |> fst) st.hand
                
                //adds new pieces to hand
                for piece in newPieces do st.hand <- MultiSet.add (fst piece) (snd piece) st.hand
                
                //add points
                st.points <- st.points + uint32 points
                                
                let st' = st 

                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                
                let st' = st // This state needs to be updated
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
        