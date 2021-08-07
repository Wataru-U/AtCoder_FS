let N = stdin.ReadLine() |> int
let A = stdin.ReadLine().Split() |> Array.map int64

let mutable pillar = [|for i in 1..N -> 0L|]

pillar.[1] <- A.[0] - A.[1] |> abs

let comp i = 
    let diffp = A.[i] - A.[i-1] |> abs
    let difft = A.[i] - A.[i-2] |> abs
    min (pillar.[i-1] + diffp) (pillar.[i-2] + difft)

let rec dp i = 
    if i = N then pillar.[N-1]
    else
        pillar.[i] <- comp i
        dp (i+1)

if N = 2 then pillar.[1]
else dp 2
|> printfn "%d"