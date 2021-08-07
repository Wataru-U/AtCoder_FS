open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text
let sw =
    new StreamWriter(Console.OpenStandardOutput())
    |> fun x -> x.AutoFlush <- false; x
Console.SetOut(sw)
let N = stdin.ReadLine() |> int
let mutable c1 = [|for i in 1..N -> 0|]
let mutable c2 = [|for i in 1..N -> 0|]
stdin.ReadLine().Split() 
|> Array.map int
|> fun x ->
    if x.[0] = 1 then
        c1.[0] <- x.[1]
    else
        c2.[0] <- x.[1]
for i in 1..(N-1) do
    let [|c;p|] = stdin.ReadLine().Split() |> Array.map int
    if c = 1 then
        c1.[i] <- c1.[i-1] + p
        c2.[i] <- c2.[i-1]
    else
        c1.[i] <- c1.[i-1]
        c2.[i] <- c2.[i-1] + p

let q = stdin.ReadLine() |> int
for i in 1..q do
    let [|l;r|] = stdin.ReadLine().Split() |> Array.map int |> Array.map (fun x -> x - 1)
    if l = 0 then 
        printfn "%d %d" (c1.[r]) (c2.[r])
    else
        printfn "%d %d" (c1.[r] - c1.[l-1]) (c2.[r] - c2.[l-1])

sw.Flush()

2804496343323.m / 3.m