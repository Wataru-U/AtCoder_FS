open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text
let MinWeight = 0.2
let MaxWeight = 1.2
let time = 20.
let randTime = 50

let rand = new Random()
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

let StrSwap (str:string) a b = 
    let mutable v = str.ToCharArray()
    let temp = v.[a]
    v.[a] <- v.[b]
    v.[b] <- temp
    new string(v)

let rec check i j x y (seen:bool[][]) (str:string) n =
    if i < 0 || i > 29 || j < 0 || j > 29 then false
    elif seen.[i].[j] = true then false
    elif i = x && j = y && n = str.Length then true
    elif n = str.Length  then false
    else
        seen.[i].[j] <- true
        match str.[n] with
        | 'U' -> check (i-1) j x y seen str (n + 1)
        | 'D' -> check (i+1) j x y seen str (n + 1)
        | 'L' -> check i (j-1) x y seen str (n + 1)
        | _ -> check i (j+1) x y seen str (n + 1)

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

type Edge(minWeight:float, maxWeight:float, maxTime:float) =
    let mutable cost = 0.
    let mutable count = 1.
    
    member this.Cost = cost
    member this.Count = count

    //エッジのコストを予測
    //誤差を修正する
    member this.CostUpdate v =
        cost <- cost + v
        count <- count + 1.

type Line(minWeight:float, maxWeight:float, maxTime:float) =
    let Edges = [|for i in 0..28 -> Edge(minWeight, maxWeight, maxTime)|]
    let mutable lineCost = 5000.

    member this.Cost index =
        max 1. (lineCost + Edges.[index].Cost)
    
    member this.CostUpdate index v =
        let t = Edges.[index].Count
        let r = max minWeight (maxWeight - 1. / maxTime * t)
        let lineRate = 0.03
        let edgeRate = 1. - lineRate
        lineCost <- lineCost + v * lineRate * r
        Edges.[index].CostUpdate (v * edgeRate * r)
    
    member this.Edge index =
        Edges.[index]


type Dijkstra (minWeight:float, maxWeight:float, maxTime:float) =
    let idx v = 
        [| (v/30); (v%30)|]
    let toNum i j = 
        i * 30 + j
    let mutable hori = 
        [|
            for i in 0..29 -> Line(minWeight,maxWeight,maxTime);
        |]
    let mutable vert = 
        [|
            for i in 0..29 -> Line(minWeight,maxWeight,maxTime);
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
        match d with
        | 0 -> vert.[j].CostUpdate (i-1) v
        | 1 -> vert.[j].CostUpdate i v
        | 2 -> hori.[i].CostUpdate (j-1) v
        | _ -> hori.[i].CostUpdate j v
    
    let graph i j d = 
        match d with
        | 0 -> (vert.[(j)].Cost (i-1))
        | 1 -> (vert.[j].Cost i)
        | 2 -> (hori.[(i)].Cost (j-1))
        | _ -> (hori.[i].Cost j)

    let Sarch start query = 
        let mutable PQ = PriorityQueue<Vertex>(4000,Vertex.nullMin,Vertex.min)
        let mutable town = [|for i in 1 .. 900 -> Vertex(i,-1.,"")|]
        town.[start] <- Vertex(start,0.,"")
        PQ.Push (town.[start])
        let condition = 10 + (max 0 (20 - (((query|>float) / (randTime|>float) * 20.) |> int )))
        while (PQ.Length <> 0) do
            let v = PQ.Pop
            let index = v.Index
            let pos = idx v.Index
            let i = pos.[0]
            let j = pos.[1]
            if v.Index >= 0  

            then
                if v.Cost = town.[v.Index].Cost
                    then 
                        let Add eCost nextIdx d = 
                            if (town.[nextIdx].Cost > (v.Cost + eCost)) || town.[nextIdx].Cost = -1.
                            then
                                let nc = eCost + v.Cost
                                town.[nextIdx] <- Vertex(nextIdx, nc, (Dir d))
                                PQ.Push (town.[nextIdx])
                            else ()
                        if i <> 0 then // U 
                            Add (vert.[(j)].Cost (i-1)) (index - 30) 0
                        if i <> 29 then // UD
                            Add (vert.[(j)].Cost i) (index + 30) 1
                        if j <> 0 then // L
                            Add (hori.[(i)].Cost (j-1)) (index - 1) 2
                        if j <> 29 then // R 
                            Add (hori.[(i)].Cost j) (index + 1) 3
        town

    let next v i j = 
            match v with 
            | "U" -> [|(i+1);j|]
            | "D" -> [|(i-1);j|]
            | "L" -> [|i;(j+1)|]
            | _ -> [|i;(j-1)|]
    
    let rec culc (town:Vertex[]) x y i j v =  
        if x = i && y = j then v
        else
            let [|ni;nj|] = next town.[toNum i j].Prev i j
            culc town x y ni nj (town.[toNum i j].Prev + v)
    
    let rec update (str:string) x y i j v n = 
        if n = -1 then ()
        else
            let [|ni;nj|] = next (str.[n].ToString()) i j
            let d = Dir2Num (str.[n].ToString())
            CostUpdate ni nj d v
            update str x y ni nj v (n-1)

    let rec expectedSum i j (str:string) n diff v = 
       if n = diff then v
        else
            let [|ni;nj|] = 
                match str.[n] with
                | 'U' -> [|(i-1);j|]
                | 'D' -> [|(i+1);j|]
                | 'L' -> [|i;(j-1)|]
                | _ -> [|i;(j+1)|]
            let d = Dir2Num (str.[n].ToString())
            expectedSum ni nj str (n+1) diff (v + (graph i j d))

           
    member this.Culc (x:int[]) query =
        let mutable result = "" 
        if query < 60 then
            result <- ""
            let ud = x.[0] - x.[2]
            let lr = x.[1] - x.[3]
            let dirUD = if ud > 0 then "U" else "D"
            let dirLR = if lr > 0 then "L" else "R"
            for i in 1..(abs ud) do
                result <- result + dirUD
            for i in 1..(abs lr) do
                result <- result + dirLR
        else
            let start =  toNum x.[0] x.[1]
            let End = toNum x.[2] x.[3]
            let town = Sarch start query
            culc town x.[0] x.[1] x.[2] x.[3] ""
            |> fun x ->
                result <- x
            (*
        if query < randTime then
            let a = rand.Next(result.Length)
            let b = rand.Next(result.Length)
            result <- StrSwap result a b
            let seen = [|for i in 0..29 -> [|for j in 0..29 -> false|]|]
            if check x.[0] x.[1] x.[2] x.[3] seen result 0
                then
                    ()
                else
                    result <- StrSwap result a b
                    *)
        
        let diff = result.Length
        let Sum = expectedSum x.[0] x.[1] result 0 diff 0.
        Print result
        let Val = stdin.ReadLine() |> float
        update result x.[0] x.[1] x.[2] x.[3] ((Val - Sum) / (diff|>float)) (result.Length-1)


let dik = Dijkstra(MinWeight,MaxWeight,time)

//回答
let rec ans n = 
    if n = 1000 then ()
    else
        stdin.ReadLine().Split()
        |> Array.map int
        |> fun x -> dik.Culc x n
        ans (n+1)

ans 0