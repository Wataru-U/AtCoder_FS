open System.Collections.Generic
open System.Collections
open System

type Person(index:int, cost:int)= 
    member this.Index = index
    member this.Cost = cost

    interface IComparable with
        member this.CompareTo yobj = 
            match yobj with
            | :? Person as x -> 
                let v = compare this.Cost x.Cost
                if v <> 0 then v
                else compare -this.Index -x.Index
            | _ -> invalidArg "yobj" "cannot compare value of different types"

let Compare a b = 
    if a = b then 0
    elif (a = 'G' && b = 'C') || (a = 'C' && b = 'P') || (a = 'P' && b = 'G') then 1
    else -1

let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
let People = 
    [|
        for i in 1..2*N -> stdin.ReadLine()
    |]

let mutable v = [|for i in 1..2*N -> Person(i-1,0)|]

for j in 0..M-1 do
    for i in 0..N-1 do
        let p1 = v.[2 * i]
        let p2 = v.[2 * i + 1]
        let winner = Compare (People.[p1.Index].[j]) (People.[p2.Index].[j])
        if winner = 1
        then 
            v.[2 * i] <- Person(p1.Index,1 + p1.Cost)
        elif winner = -1
        then
            v.[2 * i + 1] <- Person(p2.Index,1 + p2.Cost)
    v <- 
        Array.sort v
        |> Array.rev

for i in v do
    printfn "%d" (i.Index+1)