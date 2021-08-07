open System.Collections.Generic
open System.Collections
open System
open System.IO

type Company(x:int,y:int, cost:int,num:int)= 
    member this.X = x
    member this.Y = y
    member this.Cost = cost
    member this.Num = num

    interface IComparable with
        member this.CompareTo yobj = 
            match yobj with
            | :? Company as x -> compare this.X x.X
            | _ -> invalidArg "yobj" "cannot compare value of different types"

let n = stdin.ReadLine() |> int
let mutable (H:list<Company>[]) = [|for i in 0..9999 -> []|]

let rec height n v = 
    if n = 10000 then v
    elif H.[n].Length = 0 then height (n+1) (v+1) else v

for i in 1 .. n do
    stdin.ReadLine().Split()
    |> Array.map int
    |> fun x -> 
        H.[x.[1]] <- Company(x.[0],x.[1],x.[2],i) :: H.[x.[1]]
H <- [|for i in 0 .. 9999 -> H.[i] |> List.sort|]

let mutable (ans:int[][]) = [|for i in 0..n -> [||]|]

let mutable maxHeight = 0
for i in 0 .. 9999 do
    if H.[i].Length <> 0 then
        let h = height (i+1) 1
        let mutable nextMaxH = 0

        let update n x = 
            ans.[n] <- x
            nextMaxH <- max x.[3] nextMaxH

        let upperSpace = i - maxHeight
        for j in 0..(H.[i].Length-1) do
            let v = H.[i].Item(j)
            let mutable b = v.X
            let mutable left = 0
            
            if j <> 0 
                then 
                    b <- b - H.[i].[j-1].X
                    left <- H.[i].[j-1].X + 1
            if j = H.[i].Length-1
                then 
                    //上下両方の空きスペースでも足りない時
                    if ((upperSpace + h) * (10000-left)) < v.Cost 
                        then update v.Num [|(left);(maxHeight);10000;v.Y+h|] 
                    elif (10000-v.X) >= v.Cost 
                        then update v.Num [|(v.X);(v.Y);v.X+v.Cost;v.Y+1|]
                    elif (v.X+1 - left) >= v.Cost 
                        then update v.Num [|(v.X-v.Cost+1);(v.Y);v.X+1;v.Y+1|]
                    elif (10000 - left) >= v.Cost    
                        then update v.Num [|(10000-v.Cost);(v.Y);10000;v.Y+1|]
                    else 
                        let takasa = v.Cost / (10000 - left)
                        if takasa <= upperSpace+1 
                            then update v.Num [|(left);(v.Y-takasa+1);10000;v.Y+1|]
                            else update v.Num [|(left);(maxHeight);10000;maxHeight+takasa|]

                else
                    let right = v.X + 1
                    if ((upperSpace + h) * (right-left)) < v.Cost 
                        then update v.Num [|left;(maxHeight);right;v.Y+h|]
                    elif (right - left) >= v.Cost 
                        then update v.Num [|(right-v.Cost);(v.Y);right;v.Y+1|]
                        else 
                        let takasa = v.Cost / (right - left)
                        if takasa <= upperSpace+1 
                            then update v.Num [|(left);(v.Y-takasa+1);right;v.Y+1|]
                            else update v.Num [|(left);(maxHeight);right;maxHeight+takasa|]
        maxHeight <- nextMaxH  

for i in 1..n do
    let item = ans.[i]
    printfn "%d %d %d %d" item.[0] item.[1] item.[2] item.[3]



open System.Collections.Generic
open System.Collections
open System
open System.IO

type Number(v:string)= 
    let num = v |> int
    let zero = 
        num |> string |> (fun x -> v.Length - x.Length)

    member this.Num = num
    member this.Zero = zero
    member this.Val = v
        

    interface IComparable with
        member this.CompareTo yobj = 
            match yobj with
            | :? Number as x -> if this.Num = x.Num then -(compare this.Zero x.Zero) else compare this.Num x.Num
            | _ -> invalidArg "yobj" "cannot compare value of different types"

let n = stdin.ReadLine() |> int64
[|for i in 1..n -> stdin.ReadLine() |> (fun x -> Number(x))|]
|> Array.sort
|> fun x -> 
    for i in x do
        printfn "%s" i.Val