type Vertex ={
        position: int
    }
type HalfEdge =
    {   vert: int
        mutable next: int
        mutable prev: int
        pair: Adress
        face: int   }

    member this.Vert = this.vert
    member this.Next = this.next
    member this.NextUpdate n = this.next <- n
    member this.Prev = this.prev
    member this.PrevUpdate n = this.prev <- n

    static member empty=
    {
        vert = 0
        next = 0
        prev = 0
        pair = {x = 0;y = 0}
        face = 0
    }

and Face =
    {  mutable halfEdges: HalfEdge[]
       start: int
       num: int 
       mutable count: int}

    member this.HalfEdges = this.halfEdges
    member this.Start = this.start
    member this.Num = this.num
    member this.Count = this.count

    member this.Insert p v = 
        let pre = this.halfEdges.[p]
        let nextNum = pre.Next
        let nex = this.halfEdges.[nextNum]
        this.halfEdges.[this.count] <- {vert = v;next = nextNum;prev = p;pair = {x = this.num;y = this.Count};face = this.num}
        this.halfEdges.[p].NextUpdate this.count
        this.halfEdges.[nextNum].PrevUpdate this.count
        this.count <- this.count+1
and Mesh = {
       faces: Face[]
    }
and Adress =
    {   x: int
        y:int
    }

    member this.X = this.x
    member this.Y = this.y
  
let [|N;M|] = [|100;10|]
let mutable (f:HalfEdge[]) = [|for i in 0..14 -> HalfEdge.empty|]

let mutable vertex = 1
let mutable pre = 0
for i in 0 .. 9 do
    let line = {vert = i;next = i+1;prev = pre;pair = {x = 0;y = i};face = 0}
    f.[i] <- line
let last = {vert = f.[9].next; next = 0;prev = f.[9].vert;pair = {x = 0;y = 10};face = 0}
f.[10] <- last
let face = {halfEdges = f;start = 0;num = 0;count = 11}

face.Insert 4 11


let rec find (v:Face) num =
    let edge = v.HalfEdges.[num]
    printfn "%d" edge.Vert
    if(v.Start <> edge.Next)
        then find v edge.Next

find face 0

let a = [|3.0f;4.0f;5.0f|] 
let b = [|4.0f;2.0f;7.0f|]
 
let a_ = [|1.0f;a.[1]/a.[0];a.[2]/a.[0]|]
let g = b.[0]
let b_  = [|0.0f;b.[1] - g * a.[1];b.[2] - g * a.[2]|]
let b__ = [|0.0f;1.0f;b_.[2]/b.[1]|]
let a__ = [|1.0f;0.0f;a_.[2] - b__.[2] * a_.[1]|]
    
let nob = [0.0f;1.0f / (Array.map (fun x -> x * x) b__ |> Array.sum |> sqrt)]
let noa = [1.0f / (Array.map (fun x -> x * x) a__ |> Array.sum |> sqrt);0.0f]

[<Class>]
type Explicit =
    val mutable a : int
    new (data) = {a = data;}

let ex = new Explicit(10);;
ex.a <- 100
System.Console.Write(ex.a);;
