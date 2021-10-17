open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text

let T = stdin.ReadLine() |> int
for i in 1..T do
    let In = stdin.ReadLine().Split() |> Array.map int |> Array.sort
    let [|R;G;B|] = In.[..2]

    let r = if G % 3 = B % 3 then (max G B) else -1
    let g = if R % 3 = B % 3 then (max R B) else -1
    let b = if R % 3 = G % 3 then (max R G) else -1
    
    [|r;g;b|]
    |> Array.filter (fun x -> x <> -1)
    |> fun x ->
        if x.Length = 0 then -1
        else (Array.min x)
    |> printfn "%d"
    


