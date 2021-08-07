open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text

let sw =
    new StreamWriter(Console.OpenStandardOutput())
    |> fun x -> x.AutoFlush <- false; x
Console.SetOut(sw)

let [|n;K|] = stdin.ReadLine().Split() |> Array.map int64
let A = stdin.ReadLine().Split() |> Array.map int64

let N = n |> int

let t = K / n
let amari = K % n |> int

let condition = 
    if amari = 0 then 0L
    else
        A 
        |> Array.sort
        |> fun x -> x.[amari - 1]

for i in A do
    if i <= condition then  t + 1L
    else t
    |> printfn "%d" 

sw.Flush()