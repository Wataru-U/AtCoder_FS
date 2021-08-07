open System.Collections.Generic
open System.Collections
open System
open System.Diagnostics
let n = stdin.ReadLine() |> int64
let max = n  |> float |> (fun x -> sqrt x) |> int |> fun x -> x + 1
let pow (a:int64) b = 
    let mutable v = 1L
    for i in 1..b do 
       v <- v * a 
    v
let mutable boo = [|for i in 2 .. max + 1000 -> false|]
let mutable lis = new List<int64>()
let rec culc (v:int64) (e:int64) = 
    if v * e > n
        then ()
        else
            if v * e <= (max |> int64)
                then boo.[(v * e)|>int] <- true
            lis.Add(v*e)
            culc (v*e) e

for i in 2 .. max do
    if boo.[i] = false
        then culc (i|>int64) (i|>int64)
let l = lis.Count |> int64
n-l
|>printfn "%d"

