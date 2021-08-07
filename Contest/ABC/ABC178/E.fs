
let N = stdin.ReadLine() |> int

let dis (p:int64[]) (q:int64[]) = abs(p.[0] - q.[0]) + abs(p.[0] - q.[0])

let mutable MaxDis = 0L

// 左のほうが高いかどうか
let leftH = true
let  f = stdin.ReadLine().Split(' ') |> Array.map int64
let mutable right = f
let mutable left = f

let Out (p:int64[]) = 
    if p.[0] > left.[0] && p.[0] < right.[0] 
        then if leftH = true && p.[1] < left.[1] && p.[1] > right.[1]
                    then false  
                    else if p.[1] > left.[1] && p.[1] < right.[1]
                        then false
                        else true
        else true

for i in 0..N-2 do
    let s = stdin.ReadLine().Split(' ') |> Array.map int64
    if Out s = true then
        if dis right s > dis left s
            then if dis right s > MaxDis then
                    left <- s
            else if dis left s > MaxDis then
                    right <- s
        MaxDis <- dis right left    

MaxDis |> printfn "%d"   



