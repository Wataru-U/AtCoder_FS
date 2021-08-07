open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text

// 4ケースで何故か　WA
// ビジュアライザーだと得点が出てる

// -- AutoFlufh Off -- //

new StreamWriter(Console.OpenStandardOutput())
|> fun x -> x.AutoFlush <- false; x
|> Console.SetOut

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


type Edge =
    val To : int
    val Cost : int64
    new(i,c) = {To = i;Cost = c}

type Dijkstra (N,M) =
    let mutable (graph : Edge list []) = [|for i in 1..N -> [] |]
    let mutable prev = [|for i in 1..N -> 0|]

    member this.setGraph G = graph <- G
    member this.Prev = prev

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
                        if ans.[e.To] > (v.Cost + e.Cost) || ans.[e.To] = -1L 
                            then
                                ans.[e.To] <- v.Cost + e.Cost
                                prev.[e.To] <- v.Index
                                PQ.Push (Vertex(e.To,  ans.[e.To]))
        ans


// ---- input & variables ---- //

let [|N;Si;Sj|] = stdin.ReadLine().Split() |> Array.map int

let Map = 
    [|
        for i in 1..N -> 
            stdin.ReadLine().ToCharArray()
            |> Array.map (fun x -> if x = '#' then -1 else x |> string |> int )
    |]

let mutable Sarched =
    [|
        for i in 1..N -> 
            [|for i in 1..N -> false|]
    |]
let mutable count = 0 // 見てないマスの数

// 他の道とぶつかる所　　スタートを含む
let mutable (crossRoads: (int * int) List) = new List<int * int>()

let mutable Indexes = Array.init N (fun _ -> Array.init N (fun _ -> -1))
let mutable (To: Edge list []) = [||]
let mutable startIndex = 0
let mutable endIndex = 0

// ---- Lib ---- //

let tfst x = x |> (fun (a,_,_) -> a)
let tsnd x = x |> (fun (_,a,_) -> a)
let tthd x = x |> (fun (_,_,a) -> a)

let print dir diff = 
    for i in 1..diff do
        printf "%s" dir

let rev x = 
    match x with
    | "U" -> "D"
    | "D" -> "U"
    | "L" -> "R"
    | _ -> "L"
let Move x y = 
    let mutable r = []
    do
        if x <> 0
        then if Map.[x-1].[y] <> -1 then r <- "U" :: r
        if x <> N-1 
        then if Map.[x+1].[y] <> -1 then r <- "D" :: r
        if y <> 0
        then if Map.[x].[y-1] <> -1 then r <- "L" :: r
        if y <> N-1 
        then if Map.[x].[y+1] <> -1 then r <- "R" :: r
    r

//　道を辿って交差するところを探す
let rec flow x y (dir:int[]) cost = 
    if x < 0 || x = N || y < 0 || y = N then (-1,-1,-1)
    elif Map.[x].[y] = -1 then (-1,-1,-1)
    elif Indexes.[x].[y] <> -1 then (x,y,cost) 
    else flow (x + dir.[0]) (y + dir.[1]) dir (cost + Map.[x].[y])

let rec makeRoute index = 
    if To.[index].Length <> 0 then ()
    else
        do
            for i in [|[|-1;0|];[|1;0|];[|0;-1|];[|0;1|]|] do
                let CR = crossRoads.[index]
                let nextCR = flow (fst CR + i.[0]) (snd CR + i.[1]) i 0
                if nextCR <> (-1,-1,-1) 
                then
                    let nextIndex = Indexes.[tfst nextCR].[tsnd nextCR] 
                    To.[index] <- Edge(nextIndex, tthd nextCR |> int64) :: To.[index]
                    makeRoute nextIndex

let rec Sarch x y (dir:int[]) = 
    if x = -1 || x = N || y = -1 || y = N then ()
    else 
        if Sarched.[x].[y] = false 
        then
            Sarched.[x].[y] <- true
            count <- count - 1
            Sarch (x + dir.[0]) (y + dir.[1]) dir
        else ()
let SarchAllDir x y = 
    do
        if Sarched.[x].[y] = false 
        then
            Sarched.[x].[y] <- true
            count <- count - 1
    for dir in [|[|-1;0|];[|1;0|];[|0;-1|];[|0;1|]|] do
        Sarch (x + dir.[0]) (y + dir.[1]) dir


//　とりあえず　ある程度の交差点を DFS　で回る
let rec dfs index (seen : bool []) =
    let mutable ns = seen
    ns.[index] <- false
    let CR = crossRoads.[index]
    let mutable allseen = false 
    SarchAllDir (fst CR) (snd CR)
    if count = 0 then
        endIndex <- index
        true
    elif index = startIndex 
    then
        let nextCR = crossRoads.[To.[index].[0].To]
        let xDiff = fst nextCR - fst CR
        let yDiff = snd nextCR - snd CR
        let dir = 
            if xDiff > 0 then "D" elif xDiff < 0 then "U"
            elif yDiff > 0 then "R" else "L"
        let num = xDiff + yDiff |> abs
        print dir num
        allseen <- dfs To.[index].[0].To seen
        do
            if not allseen 
            then 
                print (rev dir) num
        allseen
    else
    do
        for i in To.[index] do
            if seen.[i.To] then
                if not allseen
                then
                    let nextCR = crossRoads.[i.To]
                    let xDiff = fst nextCR - fst CR
                    let yDiff = snd nextCR - snd CR
                    let dir = 
                        if xDiff > 0 then "D" elif xDiff < 0 then "U"
                        elif yDiff > 0 then "R" else "L"
                    let num = xDiff + yDiff |> abs
                    print dir num

                    allseen <- allseen || dfs i.To seen 
                    if not allseen then print (rev dir) num
    allseen
                  

let isCross (x: string list) = 
    let toNum s =
            match s with 
            | "U" | "D" -> 1
            | _ -> 2
    toNum x.[0] <> toNum x.[1]


// ---- preprocessing ---- //

for i in 0..N-1 do
    for j in 0..N-1 do
        if Map.[i].[j] <> -1 
        then 
            count <- count + 1
            let movalbe = Move i j
            if movalbe.Length > 2 || (i = Si && j = Sj) 
            then 
                Indexes.[i].[j] <- crossRoads.Count
                if i = Si && j = Sj then startIndex <- crossRoads.Count
                crossRoads.Add (i,j)
            if movalbe.Length = 2 
            then
                if isCross movalbe
                then
                    Indexes.[i].[j] <- crossRoads.Count
                    if i = Si && j = Sj then startIndex <- crossRoads.Count
                    crossRoads.Add (i,j)
        else
            Sarched.[i].[j] <- true

To <- Array.init crossRoads.Count (fun _ -> [])

makeRoute 0

let mutable seen = Array.init  crossRoads.Count (fun _ -> true)
seen.[startIndex] <- true
dfs startIndex seen

let L = 
    To
    |> Array.map (fun x -> x.Length)
    |> Array.sum

let DK = Dijkstra(crossRoads.Count,L)
DK.setGraph To

DK.Sarch startIndex

let last = DK.Prev

let mutable idx = endIndex
let mutable next = last.[endIndex]

while idx <> startIndex do
    let CR = crossRoads.[idx]
    let nextCR = crossRoads.[next]
    let xDiff = fst nextCR - fst CR
    let yDiff = snd nextCR - snd CR
    let dir = 
        if xDiff > 0 then "D" elif xDiff < 0 then "U"
        elif yDiff > 0 then "R" else "L"
    let num = xDiff + yDiff |> abs
    print dir num
    idx <- next
    next <- last.[idx]
Console.Out.Flush()


