open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text

let sw =
    new StreamWriter(Console.OpenStandardOutput())
    |> fun x -> x.AutoFlush <- false; x
Console.SetOut(sw)

let n = stdin.ReadLine() |> int
let mutable parent = [|0..n-1|]
let mutable dist = [|for i in 1..n -> 0|]
let mutable Distance = [|for i in 0..n-1 -> []|]
let mutable disList = [|for i in 1..n  -> [0]|]
let P = stdin.ReadLine().Split() |> Array.map (fun x -> (x |> int) - 1)
for i in 2..n do
    parent.[i-1] <- P.[i-2]
    dist.[i-1] <- dist.[parent.[i-1]] + 1 
    disList.[i-1] <- i-1 :: disList.[parent.[i-1]]
for i in 0..n-1 do
    Distance.[dist.[i]] <- (i) :: Distance.[dist.[i]]

let q = stdin.ReadLine() |> int

for i in 1..q do
    let [|I;d|] = stdin.ReadLine().Split() |> Array.map int
    let index = I-1
    let mutable ans = 0
    let len = d
    let idx = len - dist.[index] 
    if idx >= 0 
    then
        for j in Distance.[d] do
            let l = disList.[j]
            if l.[idx] = index then ans <- ans + 1
    stdout.WriteLine ans
    stdout.Flush ()
