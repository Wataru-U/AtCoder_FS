[<Class>]
type UnionFind (N) = 
    let length = N

    let mutable parent = Array.init length (fun index -> index)
    let mutable size = Array.create length 1

    let rec root a = if a = parent.[a] then a else root a


    member this.Size a = size.[a]

    member this.unit a b = 
        let ra = root a
        let rb = root b
        if ra <> rb then //根が違う時繋げる
            if ra > rb //根が小さい方に繋げる
            then parent.[ra] <- rb
                 size.[rb] <- size.[rb] + size.[ra]
                 parent.[a] <- rb
            else parent.[rb] <- ra
                 size.[ra] <- size.[rb] + size.[ra]
                 parent.[b] <- ra

    member this.same a b = parent.[a] = parent.[b]

