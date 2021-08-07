let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
let A = [| for i in 1..M -> stdin.ReadLine().Split() |> Array.map int|]

let left = 
    A
    |> Array.maxBy (fun x -> x.[0])
    |> fun x -> x.[0]
let right = 
    A 
    |> Array.minBy (fun x -> x.[1])
    |> fun x -> x.[1]

(right - left + 1)
|> max 0
|> printfn "%d"