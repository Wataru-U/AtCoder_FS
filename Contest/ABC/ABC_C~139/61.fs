let [|n;K|] = stdin.ReadLine().Split() |> Array.map int64
let N = n |> int
let AB = 
    [|for i in 1..N -> stdin.ReadLine().Split() |> Array.map int64|]
    |> Array.sortBy (fun x -> x.[0])
let mutable sumArr = [|for i in AB -> i.[1]|]

for i in 1..N-1 do
    sumArr.[i] <- sumArr.[i-1] + sumArr.[i]

let rec calc index = 
    if index = N then AB.[N-1].[0]
    elif sumArr.[index] >= (K |> int64) then AB.[index].[0]
    else calc (index + 1)

calc 0
|> printfn "%d"