// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a> when 'a : comparison = Map<'a, uint32> 
    
    let empty = Map.empty<'a, uint32> 
    
    let isEmpty (s: MultiSet<'a>) = s.IsEmpty
    
    let size (s: MultiSet<'a>) =
        Map.fold (fun s _ v -> v + s) 0u s
    
    let contains a (s:MultiSet<'a>) = s.ContainsKey(a)
    
    let numItems a (s:MultiSet<'a>) =
        match a with
        | a when contains a s -> s.Item(a)
        | _ -> 0u
    
    let add a n (s: MultiSet<'a>) = 
        if Map.exists a s then s.Item(a) + 
        
    //s.Add(a, n)
    
    let addSingle a (s: MultiSet<'a>) = s.Add(a, 1u)
    
    let remove a n (s: MultiSet<'a>) =
        match a with
        | a when n < s.Item(a) -> add a (s.Item(a)-n) s
        | a -> s.Remove(a)

    let removeSingle a (s:MultiSet<'a>) =
        match a with
        | a when contains a s -> add a (s.Item(a)-1u) s
        | _ -> s
        
    let fold f acc (s:MultiSet<'a>) = Map.fold (f) acc s
    
    let foldBack f (s:MultiSet<'a>) acc = Map.foldBack (f) s acc
