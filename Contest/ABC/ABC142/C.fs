open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text

let sw =
    new StreamWriter("ans.txt")
    |> fun x -> x.AutoFlush <- false; x
Console.SetOut(sw)

let N = stdin.ReadLine() |> int
let mutable junban = [|for i in 0..N -> 0|]

stdin.ReadLine().Split()
|> Array.map int
|> Array.mapi (fun i x -> junban.[x] <- i+1)

printf "%d" junban.[1]
for i in 2..N do
    printf " %d" junban.[i]
printfn ""
stdout.Flush()
