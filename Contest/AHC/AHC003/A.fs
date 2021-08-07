open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text

let rec toStr (x:int[]) n v= 
    if n = x.Length then v
    else toStr x (n + 1) (v + " " + (x.[n]|>string))
    
let exStdIn = new System.IO.StreamReader( "out.txt" )
exStdIn |> System.Console.SetIn
// 出力の準備
let sw =
    new StreamWriter("ans.txt")
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

type Edge(index:int) =
    let mutable cost = 5000.
    let mutable count = 1.
    let mutable prev = 0
    
    member this.Cost = cost
    member this.To = index

    //エッジのコストを予測
    //平均をとる
    member this.CostUpdate v =
        let vSign =  if v > 0. then 1 elif v < 0. then -1 else 0
        cost <- max 1. (cost + v * (max 0.2 (1.2 - 1. / 100. * count) * (if prev <> vSign then 0.9 else 1.1)))
        count <- count + 1.
        prev <- vSign


type Dijkstra () =
    let idx v = 
        [| (v/30); (v%30)|]
    let toNum i j = 
        i * 30 + j
    let mutable graph = 
        [|for i in 0..29 -> 
            [|for j in 0..29 -> 
                [|
                    Edge((if i = 0 then -1 else ((i - 1) * 30 + j)));
                    Edge((if i = 29 then -1 else ((i + 1) * 30 + j)));
                    Edge((if j = 0 then -1 else ((i) * 30 + j-1)));
                    Edge((if j = 29 then -1 else ((i) * 30 + j+1)));
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
        Print (Sum |> string)
        culc town x.[0] x.[1] x.[2] x.[3] "" 0
        |> fun (x,y) ->
            diff <- y
            result <- x
        //Print (toStr x 0 "")
        //move x.[0] x.[1] 0 result x.[2] x.[3]
        Print result
        let Val = stdin.ReadLine() |> float
        update town x.[0] x.[1] x.[2] x.[3] ((Val - Sum) / (diff|>float))


let dik = Dijkstra()



//ルート探索
// 最低限のポイント
let culc (x:int[]) = 
    let sb = new System.Text.StringBuilder()
    let ud = x.[0] - x.[2]
    let lr = x.[1] - x.[3]
    let dirUD = if ud > 0 then "U" else "D"
    let dirLR = if lr > 0 then "L" else "R"
    for i in 1..(abs lr) do
        sb.Append dirLR
    for i in 1..(abs ud) do
        sb.Append dirUD

    sb.ToString() |> Print 

//回答
let rec ans n = 
    if n = 0 then ()
    else
        stdin.ReadLine().Split()
        |> Array.map int
        |> dik.Culc
        ans (n-1)

ans 1000
