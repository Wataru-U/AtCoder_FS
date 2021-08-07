let N = stdin.ReadLine() |> int
let d = 
    stdin.ReadLine().Split() 
    |> Array.map int
    |> Array.sort

let l = d.[N/2 - 1] + 1
let r = d.[N/2]

max (r - l + 1) 0
|> printfn "%d"