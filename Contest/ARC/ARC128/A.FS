open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text

new StreamWriter(Console.OpenStandardOutput())
|> fun x -> x.AutoFlush <- false; x
|> Console.SetOut

let N = stdin.ReadLine() |> int
let rate = stdin.ReadLine().Split() |> Array.map float

let gold = [|for i in 0..N -> 1.|]
let silver = [|for i in 0..N -> 0.|]
silver.[1] <- max silver.[0] (gold.[0] * rate.[0])
let goldChange = [|for i in 1..N -> false|]
let silverChange = [|for i in 1..N -> false|]

for i in 0..N-1 do
    let g = silver.[i] / rate.[i]
    let s = gold.[i] * rate.[i]
    if gold.[i] < g 
    then
        gold.[i+1] <- g
        goldChange.[i] <- true
    if silver.[i] < s
    then
        silver.[i+1] <- s
        silverChange.[i] <- true
    else silver.[i+1] <- silver.[i]
        
    
    if gold.[i+1] <> gold.[i] || silver.[i+1] <> silver.[i]
    then 
        silver.[i+1] <- silver.[i+1] / gold.[i+1]
        gold.[i+1] <- 1.

let mutable g = true
let mutable ans = [|for i in 1..N -> 0|]
for i in 0..N-1 do
    let Index = N - i - 1
    if g && goldChange.[Index]
    then 
        ans.[Index] <- 1
        g <- false
    elif not g && silverChange.[Index]
    then
        ans.[Index] <- 1
        g <- true

for i in 0..N-2 do
    printf "%d " ans.[i]
printfn "%d" ans.[N-1]


stdout.Flush()