let N = stdin.ReadLine() |> int
let A =  
    stdin.ReadLine().Split() 
    |> Array.map int
    |> Array.mapi (fun i x -> x - i |> int64)
    |> Array.sort 

let center = N / 2

let SUM a = 
    A 
    |> Array.map (fun x -> x - a |> abs)
    |> Array.sum


SUM (A.[center])
|> fun x -> 
  if N % 2 = 1 then x 
  else min (SUM A.[center + 1]) x
|> printfn "%d"
