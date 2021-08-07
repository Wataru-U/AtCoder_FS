open System.Collections.Generic
open System.Collections
open System
let [|A;B;C|] = stdin.ReadLine().Split() |> Array.map int64
let first = 
    [|for i in 0..9 -> 
        let l = List<int>()
        l.Add i
        let mutable a = (i * i) % 10

        while a <> i do
            l.Add a
            a <- (a * i) % 10
        l|]
let l = [for i in 0..9 -> first.[i].Count |> int64]
let a = A % 10L
if a % 10L = 0L then printfn "0"
else
    let l_a = l.[a|>int]
    let b = B % l_a
    let c = ((C-1L)+l.[b|>int]) % l.[b|>int]
    let b_ = first.[b|>int].[c|>int] |> int64
    let a_ = ((b_-1L)+l.[a|>int]) % l.[a|>int]  |> int
    first.[a|>int].[a_]
    |> printfn "%d" 