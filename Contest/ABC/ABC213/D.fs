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
let mutable To = [|for i in 1..N -> []|]
let mutable seen = [|for i in 1..N -> false|]
let route = [|for i in 1..N-1 -> stdin.ReadLine().Split() |> Array.map int |> Array.map (fun x -> x-1)|]
for i in route do
    To.[i.[0]] <- i.[1] :: To.[i.[0]]
    To.[i.[1]] <- i.[0] :: To.[i.[1]]

To <- To |> Array.map (fun x -> List.sort x)



let rec dfs index = 
    printf "%d " (index + 1)
    let mutable c = 0
    do
        for i in To.[index] do 
            if not seen.[i]
            then
                seen.[i] <- true
                dfs i
                printf "%d " (index + 1)
                c <- c + 1
                
seen.[0] <- true
dfs 0
printfn ""

sw.Flush()