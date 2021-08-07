open System.Collections.Generic
open System.Collections
open System


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


type Vertex(index:int, cost:int64)= 
    member this.Index = index
    member this.Cost = cost

    static member nullMin = Vertex(0,1000000000000000L)
    static member nullMAx = Vertex(0,-1000000000000000L)

    static member max (a:Vertex) (b:Vertex) =  a > b 
    static member min (a:Vertex) (b:Vertex) =  a < b 

    interface IComparable with
        member this.CompareTo yobj = 
            match yobj with
            | :? Vertex as x -> compare this.Cost x.Cost
            | _ -> invalidArg "yobj" "cannot compare value of different types"

let _null = Vertex(0,100000000L)
let PQ = PriorityQueue<Vertex>(3,_null,Vertex.min)

type Edge =
    val To : int
    val Start : int64
    val Cost : int64
    new(i,s,c) = {To = i;Start = s;Cost = c}

type Dijkstra (N,M) =
    let mutable graph = [|for i in 1..N -> List<Edge>() |]

    member this.gl = Array.map (fun (x:List<Edge>) -> x.Count) graph |> Array.sum

    member this.AddEdge a b start cost = graph.[a].Add(Edge(b,start,cost)) 

    member this.Sarch start = 
        let mutable PQ = PriorityQueue<Vertex>(M,Vertex.nullMin,Vertex.min)
        let mutable ans = [|for i in 1 .. N -> -1L|]
        ans.[start] <- 0L
        PQ.Push (Vertex(start,0L))
        while (PQ.Length <> 0) do
            let v = PQ.Pop
            if v.Cost = ans.[v.Index]
                then 
                    for e in graph.[v.Index] do
                        let nextTrain = if (ans.[v.Index] % e.Start) = 0L then 0L else e.Start -  (ans.[v.Index] % e.Start)
                        if ans.[e.To] > (v.Cost + e.Cost + nextTrain) || ans.[e.To] = -1L 
                            then
                                ans.[e.To] <- v.Cost + e.Cost + nextTrain
                                PQ.Push (Vertex(e.To,  ans.[e.To]))
        ans

let [|N;M;X;Y|] = stdin.ReadLine().Split() |> Array.map int
let dik = Dijkstra(N,M*2)
if M = 0 
    then printfn "-1"
    else 
        for i in 1 .. M do
            let [|A;B;cost;start|] = stdin.ReadLine().Split() |> Array.map int64
            let a = A-1L |> int
            let b = B-1L |> int
            dik.AddEdge a b start cost
            dik.AddEdge b a start cost
        let ans = dik.Sarch (X-1)
        printfn "%d" ans.[Y-1]