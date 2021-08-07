open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text
let sw =
    new StreamWriter(Console.OpenStandardOutput())
    |> fun x -> x.AutoFlush <- false; x
Console.SetOut(sw)

let [|H;W|] = stdin.ReadLine().Split() |> Array.map int
let mutable grid = [|for i in 1..H -> [|for j in 1..W -> false|]|]

let mutable parent = 
    [|
        for i in 1..H ->
        [|
            for j in 1..W -> [|(i-1);(j-1)|]
        |]
    |]

let rec root (x:int[]) = 
    if parent.[x.[0]].[x.[1]] = x then x
    else root parent.[x.[0]].[x.[1]]

let update (x:int[]) = 
    let a = x.[0]
    let b = x.[1]
    grid.[a].[b] <- true 
    let udlr = 
        [|
            [|(a-1);b|];
            [|(a+1);b|];
            [|a;(b-1)|];
            [|a;(b+1)|]
        |]
        |> Array.choose (fun x -> if (x.[0] >= 0 && x.[0] < H && x.[1] >= 0 && x.[1] < W) then
                                        if grid.[x.[0]].[x.[1]] then
                                            Some(x)
                                        else 
                                            None
                                    else
                                        None)  
    if udlr.Length <> 0
    then 
        let mutable minA = 100000
        let mutable minB = 100000
        let roots = udlr |> Array.map root
        for pos in roots do
            if pos.[0] < minA then
                minA <- pos.[0]
                minB <- pos.[1]
            elif pos.[0] = minA && pos.[1] < minB then
                minA <- pos.[0]
                minB <- pos.[1]
        let result = [|minA;minB|]
        for pos in roots do 
            parent.[pos.[0]].[pos.[1]] <- result
        for pos in udlr do
            parent.[pos.[0]].[pos.[1]] <- result
        parent.[a].[b] <- result           

let same (x:int[]) (y:int[]) = 
    (root x) = (root y) && grid.[x.[0]].[x.[1]] && grid.[y.[0]].[y.[1]]

let calc (query:int[]) = 
    if query.[0] = 0 then
        update query.[1..]
    else
        if (same (query.[1..2]) (query.[3..4])) then
            "Yes"
        else
            "No"
        |> stdout.WriteLine

let q = stdin.ReadLine() |> int

for i in 1..q do
    stdin.ReadLine().Split() 
    |> Array.map int
    |> Array.map (fun x -> x-1)
    |> calc

stdout.Flush()