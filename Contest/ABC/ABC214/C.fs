open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text

new StreamWriter(Console.OpenStandardOutput())
|> fun x -> x.AutoFlush <- false; x
|> Console.SetOut

let N = stdin.ReadLine() |> int
let S = stdin.ReadLine().Split() |> Array.map int64
let T = stdin.ReadLine().Split() |> Array.map int64

let mutable sunukes = T

let rec calc index prev = 
    if sunukes.[index] <= sunukes.[prev] + S.[prev] then ()
    else  
        sunukes.[index] <- sunukes.[prev] + S.[prev]
        calc ((index + 1) % N) index

[|for i in 0..N-1 -> (T.[i],i)|]
|> Array.sortBy fst
|> fun x ->
    for i in x do
        let index = snd i
        let v = fst i
        if sunukes.[index] >= v 
        then 
            sunukes.[index] <- v
            calc ((index + 1) % N) index

for i in sunukes do
    printfn "%d" i
stdout.Flush()