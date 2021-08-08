let [|H;W;N|] = stdin.ReadLine().Split() |> Array.map int

let fst x = x |> (fun (a,_,_) -> a)
let snd x = x |> (fun (_,a,_) -> a)
let thd x = x |> (fun (_,_,a) -> a)
let mutable card = 
    [|
        for i in 1..N -> 
            stdin.ReadLine().Split() 
            |> Array.map int
            |> fun x -> (x.[0],x.[1],i-1)
    |]

card
|> Array.sortBy fst
|> fun x -> 
    for i in 0..N-1 do
        do
            if i = 0 
            then card.[thd x.[i]] <- (i+1,snd x.[i],thd x.[i])
            elif fst x.[i] = fst x.[i-1]
            then card.[thd x.[i]] <- (fst card.[thd x.[i-1]],snd x.[i],thd x.[i])
            else card.[thd x.[i]] <- (fst card.[thd x.[i-1]] + 1,snd x.[i],thd x.[i])
card
|> Array.sortBy snd
|> fun x -> 
    for i in 0..N-1 do
        if i = 0 
            then card.[thd x.[i]] <- (fst x.[i],i+1,thd x.[i])
            elif snd x.[i] = snd x.[i-1]
            then card.[thd x.[i]] <- (fst x.[i],snd card.[thd x.[i-1]],thd x.[i])
            else card.[thd x.[i]] <- (fst x.[i],snd card.[thd x.[i-1]] + 1,thd x.[i])
for i in card do
    printfn "%d %d" (fst i) (snd i)