open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text

// 昇順
// 降順にするときは　cmd+F -> /// のところの比較演算子を変える
// 扱う型を変えるときは　Arr:int[]  の　intを変える

[<Class>]
type PriorityQueue (N) =
    let mutable (Arr:int[]) = [|1 .. N|]
    let mutable i = 0
    let l = Arr.Length

    let Swap a b = 
        let s = Arr.[a]
        Arr.[a] <- Arr.[b]
        Arr.[b] <- s

    let rec ParentCompare par num =
        if num = 0 then ()
            else if Arr.[par] > Arr.[num] ///ここを変える
                then 
                    Swap par num
                    ParentCompare ((par - 1) / 2) par

    let child num = 
        let c = 2 * (num + 1)
        if c >= i || Arr.[c] > Arr.[c-1] then c-1 else c  ///

    let rec ChildCompare num = 
        if num < i then
            let c = child num
            if c < i then
                if Arr.[num] > Arr.[c] then ///ここを変える
                    Swap num c
                    ChildCompare c

    member this.Length = i
    member this.Array = Arr

    member this.Push value = 
        if i < l then
            Arr.[i] <- value
            if i <> 0 then
                let par = (i - 1) / 2
                let num = i
                ParentCompare par num
            i <- i+1  

    member this.Pull = 
        i <- i - 1
        if i = -1 
        then -1
        else
            do
                if i > 0 then 
                    Swap i 0
                    ChildCompare 0
            Arr.[i]
let sb = System.Text.StringBuilder()

let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
let Conditions = [|for i in 1..M -> stdin.ReadLine().Split() |> Array.map int|]
let mutable ans = []

let cd_left = [|for i in 0..N -> []|]

for item in Conditions do
    cd_left.[item.[0]] <- item.[1] :: cd_left.[item.[0]]
let seen = [|for i in cd_right -> i.Length|]

let PQ = PriorityQueue(N*2)

let Push index = 
    PQ.Push index

// 左側にないといけないものがない場合キューに入れる
for i in 1..N do
    if seen.[i] = 0 
    then Push i


// おけるものの中から最小のものを入れる
let rec Pull index = 
    if index = N then ()
    else
        let v = PQ.Pull
        if v = -1 then ()
        else 
            for i in cd_left.[v] do
                seen.[i] <- seen.[i] - 1
                do
                if seen.[i] = 0 
                then Push i
            ans <- v :: ans
            do
                if index <> N-1 
                then
                    string(v) + " " |> sb.Append
                else
                    string(v) |> sb.AppendLine
            Pull (index + 1)
Pull 0
if ans.Length <> N then printfn "-1"
else
    sb.ToString() |> stdout.Write

