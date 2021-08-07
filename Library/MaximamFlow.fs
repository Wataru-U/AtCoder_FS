// 最大流

// 使い方
//      頂点　N に対して
//          MaximamFlow(N)
//      辺の追加  a -> b の有向辺
//          .Add a b (cap : int64)
//      最大流  s -> t 
//          .maxFlow s t

open System.Collections.Generic
open System.Collections
open System

//  ----- lib ------

[<Class>]
type Edge = 
    val Rev : int  // 対応する反対向きの辺
    val From : int
    val To : int
    val mutable Cap : int64 // キャパシティ

    new(r,f,t,c) = {Rev = r;From = f;To = t;Cap = c}

[<Class>]    
type Graph(n) =  // n は頂点の数
    let mutable (G : Edge List []) = [|for i in 1..n -> List<Edge>()|]

    member this.graph = G

    // 有向グラフを両方に入れる
    member this.Add f t cap = 
        G.[f].Add(Edge((G.[t].Count),f,t,cap))
        G.[t].Add(Edge((G.[f].Count - 1),t,f,0L))

    //　逆向きの辺のポインタを返す
    member this.RevEdgeCap (e:Edge)= 
        &G.[e.To].Item(e.Rev).Cap 
    

[<Class>]
type Maximamflow(N) =
    let INF = 1e9 |> int64
    let mutable (used : bool []) = [||]
    let mutable graph = Graph(N)

    let rec dfs v t f = 
        if v = t then f  // 繋がった時
        else
            used.[v] <- true

            let rec check index = 
                if index = graph.graph.[v].Count then 0L
                else
                    let e = graph.graph.[v].[index]
                    if not used.[e.To] && e.Cap > 0L  //　使われていなくて容量も残っている場合
                    then
                        let d = dfs e.To t (min f e.Cap ) 
                        if d > 0L
                            then
                                //　マッチングが成功していたら　通ったパスを更新
                                graph.graph.[v].Item(index).Cap  <- e.Cap - d
                                graph.RevEdgeCap e <- graph.RevEdgeCap e + d
                                d
                            else check (index + 1)
                    else check (index + 1)
            
            check 0


    member this.maxFlow s t = 
        let mutable flow = 0L
        let rec calc () =
            used <- Array.init N (fun _ -> false)
            let f = dfs s t INF
            flow <- flow + f
            if f = 0L // 増加パスがなくなったら
            then flow
            else calc ()
        calc ()
    
    member this.Add a b cap =
        graph.Add a b cap

// ----- end lib -----

// ----- input -----
let [|N;E|] = stdin.ReadLine().Split() |> Array.map int

let B = 
    [|
        for i in 1..E -> 
            stdin.ReadLine().Split() 
            |> Array.map int
    |]

// ----- end input -----

let G = Maximamflow(N)

for i in B do
    G.Add i.[0] i.[1] (i.[2] |> int64)

G.maxFlow 0 (N-1)
|> printfn "%d"