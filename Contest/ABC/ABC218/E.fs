let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
let Edge = 
    [|
        for i in 1..M ->
            stdin.ReadLine().Split() 
            |> fun x -> (((int x.[0])-1,(int x.[1])-1), int64 x.[2])
    |]
    |> Array.sortBy snd

let mutable sum = Array.sumBy (snd >> fun x -> max x 0L) Edge

let parent = [|for i in 1..N -> i-1|]
let childlen = [|for i in 1..N -> 1|]
let rec root index = 
    if index = parent.[index] then index else root parent.[index]

let rec calc index = 
    if childlen.[0] = N then ()
    else
        let e = Edge.[index]
        let a = fst (fst e)
        let b = snd (fst e)
        let v = snd e
        if root a = root b 
        then 
            calc (index+1)
        else
            let s = min (root a) (root b)
            let l = max (root a) (root b)
            do
                if v > 0L then sum <- sum - v
            parent.[l] <- s
            parent.[a] <- s
            parent.[b] <- s
            childlen.[s] <- childlen.[s] + childlen.[l]
            calc (index + 1)

calc 0
printfn "%d" sum