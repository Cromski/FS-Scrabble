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
    
    let add a n (s: MultiSet<'a>) : Map<'a, uint32> = 
        if contains a s then Map.add a (s.Item(a) + n) s else Map.add a n s 
    
    let addSingle a (s: MultiSet<'a>) =
        if contains a s then Map.add a (s.Item(a) + 1u) s else s.Add(a, 1u)
    
    let remove a n (s: MultiSet<'a>) =
        match a with
        | a when n < s.Item(a) -> s.Add(a, s.Item(a)-n)
        | a -> s.Remove(a)

    let removeSingle a (s:MultiSet<'a>) =
        if contains a s && s.Item(a) > 1u then s.Add(a, s.Item(a)-1u) else if contains a s then s.Remove(a) else s
        
    let fold f acc (s:MultiSet<'a>) = Map.fold (f) acc s
    
    let foldBack f (s:MultiSet<'a>) acc = Map.foldBack (f) s acc
