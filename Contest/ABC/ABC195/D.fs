open System.Collections.Generic
open System.Collections
open System
open System.Diagnostics

type Box(index:int, capacity:int)= 
    member this.Index = index
    member this.Cap = capacity

    interface IComparable with
        member this.CompareTo yobj = 
            match yobj with
            | :? Box as x -> compare this.Cap x.Cap
            | _ -> invalidArg "yobj" "cannot compare value of different types"

type Nimotu(weight:int, v:int,index:int) = 
    member this.weight = weight
    member this.Value = v
    member this.Index = index

    interface IComparable with
        member this.CompareTo yobj = 
            match yobj with
            | :? Nimotu as x -> if this.weight = x.weight then compare this.Value x.Value else compare this.weight x.weight
            | _ -> invalidArg "yobj" "cannot compare value of different types"


let [|N;M;Q|] = stdin.ReadLine().Split() |> Array.map int
let nimotu = [|for i in 1..N -> stdin.ReadLine().Split() |> Array.map int|]
let Items = [|for i in 0..N-1 -> Nimotu(nimotu.[i].[0],nimotu.[i].[1],i)|] |> Array.sort
let hako = stdin.ReadLine().Split() |> Array.map int
let Boxes = [| for i in 0..M-1 -> Box(i+1,hako.[i]) |] |> Array.sort

let query = [|for i in 1..Q -> stdin.ReadLine().Split() |> Array.map int|]
for i in query do
    let mutable b = [|for i in 0..N-1 -> true|]
    let mutable ans = 0
    
    for item in Boxes do
        if item.Index < i.[0] || item.Index > i.[1]
            then
                let mutable max = -1
                let mutable index = -1
                ()
                for j in Items do
                    if j.weight <= item.Cap && b.[j.Index] 
                        then 
                            if j.Value > max then
                                max <- j.Value
                                index <- j.Index
                if max > 1 
                    then 
                        ans <- ans + max
                        b.[index] <- false
    printfn "%d" ans