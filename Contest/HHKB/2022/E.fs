open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text

let [|N;M;Q|] = stdin.ReadLine().Split() |> Array.map int
let graph = 
    [|
        for i in 1..(M+Q) -> 
            stdin.ReadLine().Split() 
            |> Array.map int64
            |> Array.map (fun x -> x - 1L)
            |> fun x -> [|x.[0];x.[1];x.[2];(i-M-1 |> int64)|]
    |]
    |> Array.sortBy (fun x -> x.[2])
let query = [| for i in 1..Q -> false |]
let uf = [|for i in 1..N -> i-1|]
let rec root i = if uf.[i] = i then i else root uf.[i]

let rec minGraph i c = 
    if c = N then ()
    else
        let edge = graph.[i] 
        let a = edge.[0] |> int |> root
        let b = edge.[1] |> int |> root
        if a = b then minGraph (i+1) c
        elif edge.[3] >= 0L
        then
            query.[edge.[3] |> int] <- true
            minGraph (i+1) c
        else 
            uf.[(max a b)] <- min a b
            minGraph (i+1) (c+1)

minGraph 0 1

for i in query do
    if i then "Yes" else "No"
    |> printfn "%s"

stdout.Flush()
