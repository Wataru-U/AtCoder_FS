let [|N;K|] = stdin.ReadLine().Split() |> Array.map int
let A = stdin.ReadLine().Split() |> Array.map int64

let rec calc i ans sum = 
    if i = N then ans
    else
        let ns = sum - A.[i - K ] + A.[i]
        calc (i + 1) (ans + ns) (ns)

A.[0..K-1]
|> Array.sum
|> fun x -> calc K x x
|> printfn "%d"

