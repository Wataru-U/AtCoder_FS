//できてない
let T = stdin.ReadLine() |> int

let calc index = 
    if index = T then ()
    else 
        let N = stdin.ReadLine() |> int

        let event = 
            let mutable e = []
            [|
                for i in 1..N -> 
                    stdin.ReadLine().Split()
                    |> Array.map int()
                    |> fun x -> 
                        if x.[0] = x.[1] then e <- (x.[0],0)
                        else
                            e <- (x.[0],1)
                            e <- (x.[1],-1)
            |]
            e
            |> List.sortBy fst
        let rec c i v down t prev = 
            if t = 2 then "No"
            elif i = N * 2 
            then 
                if v <= 1 then "Yes" else "No"
            else 
                if fst event.[i] = prev
                then 
                    if snd event.[i] = 1 then c (i+1) (v + 1) down t prev
                    elif snd event.[i] = 0 then c (i+1) v down (t+1) prev
                    else c (i+1) v (down + 1) t prev
                else
                    if v < down then "No"
                    else
                        let mutable nv = v - 