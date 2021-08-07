let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
let A = stdin.ReadLine().Split() |> Array.map int |> Array.sort
let B = stdin.ReadLine().Split() |> Array.map int |> Array.sort

let mutable ans = 1000000000

let mutable index = 0

let rec calc i j ans = 
    if i = N || j = M then ans
    else
        let a = A.[i]
        let b = B.[j]
        if a > b then
            calc i (j+1) (min ans (a - b |> abs))
        else
            calc (i+1) j (min ans (a - b |> abs))
calc 0 0 1000000001
|> printfn "%d"