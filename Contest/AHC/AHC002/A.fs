open System.Diagnostics
open System.Collections.Generic
open System.Linq


type Route = 
    val r : string
    val p : int
    new(str,i) = {r = str;p = i}

type patern = 
    val Seen : bool[]
    val x : int
    val y : int
    val route : Route
    new(b,X,Y,r) = {Seen = b;x = X;y = Y;route = r}
let sw = Stopwatch()
sw.Start()

// in
let mutable [|Y;X|] = stdin.ReadLine().Split() |> Array.map int
let tile = [|for i in 1..50 -> stdin.ReadLine().Split() |> Array.map int |]
let seen = 
    Array.map (fun x -> Array.max x) tile
    |> Array.max
    |> fun x -> [|for i in 0..x+1 -> false|]
let p = [|for i in 1..50 -> stdin.ReadLine().Split() |> Array.map int |]

let see x y (Seen:bool[]) =
    let mutable s = 
    s.[tile.[y].[x]] <- true
    s
// 行けるかどうか
let leftArrival x y (Seen:bool[]) =
    if x = 0 then false
    else not Seen.[tile.[y].[x-1]]

let rightArrival x y (Seen:bool[]) =
    if x = 49 then false
    else not Seen.[tile.[y].[x+1]]

let upArrival x y (Seen:bool[]) =
    if y = 0 then false
    else not Seen.[tile.[y-1].[x]]

let downArrival x y (Seen:bool[]) =
    if y = 49 then false
    else not Seen.[tile.[y+1].[x]] 

let Movable x y (Seen:bool[]) =
    (not(not(leftArrival x y Seen) && not(rightArrival x y Seen) && not(upArrival x y Seen) && not(downArrival x y Seen)))

//　移動
let goLeft x y (Seen:bool[]) (queue:List<patern>) = 
    if leftArrival x y Seen then
        queue.Add(patern((see (x-1) y Seen),(x-1),y,Route(queue.[0].route.r + "L",queue.[0].route.p + p.[y].[x-1])))
        queue
    else
        queue

let goRight x y (Seen:bool[]) (queue:List<patern>) = 
    if rightArrival x y Seen then
        queue.Add(patern((see (x+1) y Seen),(x+1),y,Route(queue.[0].route.r + "R",queue.[0].route.p + p.[y].[x+1])))
        queue
    else
        queue

let goUp x y (Seen:bool[]) (queue:List<patern>) = 
    if upArrival x y Seen then
        queue.Add(patern((see (x) (y-1) Seen),(x),y-1,Route(queue.[0].route.r + "U",queue.[0].route.p + p.[y-1].[x])))
        queue
    else
        queue

let goDown x y (Seen:bool[]) (queue:List<patern>) = 
    if downArrival x y Seen then
        queue.Add(patern((see (x) (y+1) Seen),(x),y+1,Route(queue.[0].route.r + "D",queue.[0].route.p + p.[y+1].[x])))
        queue
    else
        queue
let go x y (Seen:bool[]) (queue:List<patern>) =
    goLeft x y Seen queue
    |> fun z -> goRight x y Seen z
    |> fun z -> goUp x y Seen z
    |> fun z -> goDown x y Seen z
//　計算
let rec culc (queue:List<patern>) (ans:Route)  =
    if sw.Elapsed.Milliseconds >= 501 && sw.Elapsed.Seconds >= 4 then printfn "%s" ans.r
    else
        let q = 
            queue.[0]
            |> fun z -> go z.x z.y z.Seen queue
        if q.Count = 1 
        then 
            if ans.p >= queue.[0].route.p then printfn "%s" ans.r
            else printfn "%s" queue.[0].route.r
        else
            let na = if ans.p >= queue.[0].route.p then ans else queue.[0].route
            q.RemoveAt(0)
            culc q na

//　処理
List<patern>()
|> fun x -> x.Add(patern((see X Y seen),X,Y,Route("",p.[X].[Y]))); x
|> fun x -> culc x (Route("",p.[X].[Y]))