let [|N;c|] = stdin.ReadLine().Split() |> Array.map int
let C = c |> int64
let query = 
    [|
        for i in 1..N ->
            stdin.ReadLine().Split()
            |> Array.map int64
    |]

let mutable ans = 0L
let mutable Sum = 0L
let mutable day = 0L
let str = [|for i in query -> (i.[0],i.[2])|]
let e = [|for i in query -> (i.[1] + 1L,-i.[2])|]

Array.append str e
|> Array.sortBy (fst)
|> fun x ->
    for i in x do
        if day <> fst i 
        then
            if snd i < 0L then
                ans <- ans + (min Sum C) * ((fst i) - day)
            else
                ans <- ans + (min Sum C) * ((fst i )- day)
            day <- fst i
        Sum <- Sum + snd i
       


printfn "%d" ans