module internal usefulMethods

    
    let changeCoords xWordBool subtract (x_coordinate, y_coordinate)=
        match xWordBool with
        | true -> if subtract then (x_coordinate - 1, y_coordinate) else (x_coordinate + 1, y_coordinate)
        | _ -> if subtract then (x_coordinate, y_coordinate - 1) else (x_coordinate, y_coordinate + 1)

    let rec startTileOfWord coordinates xWordBool board =
        
        let (x_coordinate, y_coordinate) = coordinates
        let newCoords =
            match xWordBool with
            | true -> (x_coordinate - 1, y_coordinate)
            | _ -> (x_coordinate, y_coordinate - 1)

        match Map.tryFind newCoords board with
        | None -> coordinates
        | Some _ -> startTileOfWord newCoords xWordBool board

    