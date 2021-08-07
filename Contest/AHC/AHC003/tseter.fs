open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text
let rec toStr (x:int[]) n v= 
    if n = x.Length then v
    else toStr x (n + 1) (v + " " + (x.[n]|>string))
    
// 出力の準備
let sw =
    new StreamWriter(Console.OpenStandardOutput())
    |> fun x -> x.AutoFlush <- false; x
Console.SetOut(sw)

let Print (x:string) =
    x |> stdout.WriteLine
    stdout.Flush ()

let rec move i j n (v:string) x y = 
    if n = v.Length then Print((toStr [|i;j|] 0 "") + (if (i = x && j = y) then " true" else " false"))
    else
        match v.[n] with
        | 'D' -> move (i+1) j (n+1) v x y 
        | 'U' -> move (i-1) j (n+1) v x y
        | 'L' -> move i (j-1) (n+1) v x y 
        | _ -> move i (j+1) (n+1) v x y

//配列の要素がnullだと怒られるので代わりになるものを入れておく
type PriorityQueue<'T when 'T :> IComparable>(capacity: int, _null: 'T, comp: 'T -> 'T -> bool) = 
    let mutable (array: 'T[]) = [|for i in 1..capacity -> _null|]
    let mutable length = 0
    let parent index = (index - 1) / 2

    let swap a b = 
        let c = array.[b]
        array.[b] <- array.[a]
        array.[a] <- c

    let rec push index = 
        let P = parent index
        match index with
        | 0 -> ()
        | _ -> 
            match comp array.[index] array.[P] with
            | false -> ()
            | _ ->
                swap index (P)
                push (index-1)
   
    let rec pop index = 
        if index >= length ||  (index * 2 + 1) >= capacity
            then ()
            else
                //優先度が高いほうにする
                let child = 
                    if comp array.[index * 2 + 1] array.[(index + 1) * 2] 
                        then index * 2 + 1 
                        else (index + 1) * 2
                if comp array.[child] array.[index] && child < length
                    then
                        swap child index
                        pop child
                    else ()

    member this.Length = length

    member this.Push v = 
        let index = length
        array.[index] <- v
        push index
        length <- length+1
        
     
    member this.Pop = 
        swap (length-1) 0
        length <- length-1
        pop 0
        array.[length]


type Vertex(index:int, cost:float, prev:string)= 
    member this.Index = index
    member this.Cost = cost
    member this.Prev = prev

    static member nullMin = Vertex(0,1000000000000000.,"")
    static member nullMAx = Vertex(0,-1000000000000000.,"")

    static member max (a:Vertex) (b:Vertex) =  a > b 
    static member min (a:Vertex) (b:Vertex) =  a < b 

    interface IComparable with
        member this.CompareTo yobj = 
            match yobj with
            | :? Vertex as x -> compare this.Cost x.Cost
            | _ -> invalidArg "yobj" "cannot compare value of different types"

let _null = Vertex(0,100000000.,"")

type Edge(index:int, minWeight:float, maxWeight:float, maxTime:float) =
    let mutable cost = 5000.
    let mutable count = 1.
    
    member this.Cost = cost
    member this.To = index

    //エッジのコストを予測
    //誤差を修正する
    member this.CostUpdate v =
        cost <- max 1. (cost + v * (max minWeight (maxWeight - 1. / maxTime * count)))
        count <- count + 1.


type Dijkstra (minWeight:float, maxWeight:float, maxTime:float) =
    let idx v = 
        [| (v/30); (v%30)|]
    let toNum i j = 
        i * 30 + j
    let mutable graph = 
        [|for i in 0..29 -> 
            [|for j in 0..29 -> 
                [|
                    Edge((if i = 0 then -1 else ((i - 1) * 30 + j)),minWeight,maxWeight,maxTime);
                    Edge((if i = 29 then -1 else ((i + 1) * 30 + j)),minWeight,maxWeight,maxTime);
                    Edge((if j = 0 then -1 else ((i) * 30 + j-1)),minWeight,maxWeight,maxTime);
                    Edge((if j = 29 then -1 else ((i) * 30 + j+1)),minWeight,maxWeight,maxTime);
                |] 
            |] 
        |]
    
    let Dir v = 
        match v with
        | 0 -> "U"
        | 1 -> "D"
        | 2 -> "L"
        | _ -> "R"
    
    let Dir2Num v = 
        match v with
        | "U" -> 0
        | "D" -> 1
        | "L" -> 2
        | _ -> 3
    
    let CostUpdate i j d v = 
        graph.[i].[j].[d].CostUpdate v

    let Sarch start = 
        let mutable PQ = PriorityQueue<Vertex>(4000,Vertex.nullMin,Vertex.min)
        let mutable town = [|for i in 1 .. 900 -> Vertex(i,-1.,"")|]
        town.[start] <- Vertex(start,0.,"")
        PQ.Push (town.[start])
        while (PQ.Length <> 0) do
            let v = PQ.Pop
            let pos = idx v.Index
            let i = pos.[0]
            let j = pos.[1]
            if v.Index >= 0 
            then
                if v.Cost = town.[v.Index].Cost
                    then 
                        for dir in 0..3 do
                            let e = graph.[i].[j].[dir]
                            if e.To >= 0 
                            then
                                if town.[e.To].Cost > (v.Cost + e.Cost) || town.[e.To].Cost = -1. 
                                    then
                                        let d = Dir dir
                                        let nc = e.Cost + v.Cost
                                        town.[e.To] <- Vertex(e.To, nc,d)
                                        PQ.Push (town.[e.To])
        town

    let next v i j = 
            match v with 
            | "U" -> [|(i+1);j|]
            | "D" -> [|(i-1);j|]
            | "L" -> [|i;(j+1)|]
            | _ -> [|i;(j-1)|]
    
    let rec culc (town:Vertex[]) x y i j v diff= 
        if x = i && y = j then (v,diff)
        else
            let [|ni;nj|] = next town.[toNum i j].Prev i j
            culc town x y ni nj (town.[toNum i j].Prev + v) (diff + 1)
    
    let rec update (town:Vertex[]) x y i j v = 
        if x = i && y = j then ()
        else
            let [|ni;nj|] = next town.[toNum i j].Prev i j
            let d = Dir2Num town.[toNum ni nj].Prev
            CostUpdate ni nj d v
            let ad = 
                match d with 
                | 0 -> 1 
                | 1 -> 0
                | 2 -> 3
                | _ -> 2
            CostUpdate i j ad  v
            update town x y ni nj v

           
    member this.Culc (x:int[]) = 
        let start =  toNum x.[0] x.[1]
        let End = toNum x.[2] x.[3]
        let town = Sarch start
        let mutable result = ""
        let mutable diff = 0
        let Sum = town.[End].Cost
        culc town x.[0] x.[1] x.[2] x.[3] "" 0
        |> fun (x,y) ->
            diff <- y
            result <- x
        //Print (toStr x 0 "")
        //move x.[0] x.[1] 0 result x.[2] x.[3]
        (result, Sum, diff,town)
    
    member this.Reculc v Sum diff town (x:int[]) = 
        update town x.[0] x.[1] x.[2] x.[3] ((v - Sum) / (diff|>float))




let rec CulcPoint (hori:float[][]) (vert:float[][]) i j x y v (str:string) n =
    if i = x & j = y then v
    else
    match str.[n] with
    | 'U' -> CulcPoint hori vert (i-1) j x y (v + (vert.[i-1].[j])) str (n+1)
    | 'D' -> CulcPoint hori vert (i+1) j x y (v + (vert.[i].[j])) str (n+1)
    | 'L' -> CulcPoint hori vert i (j-1) x y (v + (hori.[i].[j-1])) str (n+1)
    | _ -> CulcPoint hori vert i (j+1) x y (v + (hori.[i].[j])) str (n+1)

let teisuu = pown 0.998 1000



let mutable MaxPoint = 0.
let mutable highestMinWeight = 0
let mutable highestMaxWeight = 0
let mutable highestMaxTime = 0
()
for minWeight in 2..10 do
    let MinWeight = (minWeight |> float) / 10.
    for maxWeight in (10)..20 do
        let MaxWeight = (maxWeight |> float) / 10.
        for maxTime in 20..20 do
            let MaxTime = maxTime |> float
            let mutable point = 0.
            ()

            for case in 0..99 do
                let pathNum = if case < 10 then ("000" + case.ToString()) else ("00" + case.ToString() )
                let path = "AHC/AHC003/in/" + pathNum + ".txt"
                let exStdIn = new System.IO.StreamReader(path)
                exStdIn |> System.Console.SetIn
                let hori = [|for i in 0..29 -> stdin.ReadLine().Split() |> Array.map float|]
                let vert = [|for i in 0..28 -> stdin.ReadLine().Split() |> Array.map float|]
                let Dik = Dijkstra(MinWeight,MaxWeight,MaxTime)
                let mutable t = teisuu
                for query in 1..1000 do
                    t <- t / 0.998
                    let In = stdin.ReadLine().Split() |> Array.map float
                    let start2end = In.[..3] |> Array.map int
                    
                    let mutable diff = 0
                    let mutable result = ""
                    let mutable Sum = 0.
                    let mutable (town:Vertex[]) = [||]
                    
                    Dik.Culc start2end
                    |> fun (x,y,z,w) ->
                        result <- x
                        Sum <- y
                        diff <- z
                        town <- w
                    
                    let b = CulcPoint hori vert start2end.[0] start2end.[1]  start2end.[2]  start2end.[3] 0. result 0
                    let answer = b * In.[5]  //返ってくる答え
                    Dik.Reculc answer Sum diff town start2end

                    point <- point + 2312311. * t * In.[4] / b



                printfn "end %d %d %d %d %f %f" case minWeight maxWeight maxTime point MaxPoint
                Print ""
                if point > MaxPoint then 
                    MaxPoint <- point
                    highestMinWeight <- minWeight
                    highestMaxWeight <- maxWeight
                    highestMaxTime <- maxTime
printfn "%f %d %d %d" MaxPoint highestMinWeight highestMaxWeight highestMaxTime
Print ""