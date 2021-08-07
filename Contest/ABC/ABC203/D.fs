let [|N;K|] = stdin.ReadLine().Split() |> Array.map int
let grid = [|for i in 1..N -> stdin.ReadLine().Split() |> Array.map int|]
let sorted =
    [|
        for i in 0..(N-1) do
            for j in 0..(N-1) ->
                [|grid.[i].[j];i;j|]
    |]
    |> Array.sort

let center = K * K - ( K * K / 2)
let mutable seen = [|for i in 1..N -> [|for j in 1..N -> false|]|]

let rec junnbi n = 
    if n = center - 1 then ()
    else
        seen.[sorted.[n].[1]].[sorted.[n].[2]] <- true
        junnbi (n+1)

junnbi 0

let Sarch i j =
    

let rec culc n =
    if 
    then sorted.[n].[0]
    else
        seen.[sorted.[n].[1]].[sorted.[n].[2]]
        culc (n+1)