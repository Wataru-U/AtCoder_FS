// ２部最大マッチング

// 使い方
//      (n,m) の組み合わせに対して
//
//          BipartiteMatching(N,N)
//
//      辺の入れ方
//        頂点    n   m
//          s -- 0   0  -- t
//               1   1 
//               :   :
//               :   :
//          に対して
//
//          .Add(ni,mi)
//
//      最大マッチング
//          .maxFlow

open System.Collections.Generic
open System.Collections
open System

//  ----- lib ------

[<Class>]
type Edge = 
    val Rev : int  // 対応する反対向きの辺
    val From : int
    val To : int
    val mutable Cap : int // キャパシティ

    new(r,f,t,c) = {Rev = r;From = f;To = t;Cap = c}

// 2部最大マッチング用
[<Class>]    
type Graph(n) =  // n は頂点の数
    let mutable (G : Edge List []) = [|for i in 1..n -> List<Edge>()|]

    member this.graph = G

    // 有向グラフを両方に入れる
    // マッチングしたかどうかなので cap = 0 or 1
    member this.Add f t = 
        G.[f].Add(Edge((G.[t].Count),f,t,1))
        G.[t].Add(Edge((G.[f].Count - 1),t,f,0))

    //　逆向きの辺のポインタを返す
    member this.RevEdgeCap (e:Edge)= 
        &G.[e.To].Item(e.Rev).Cap 
    
// (2,2)　の場合のグラフ
// 0 --  1      3  -- tail
//   \      --      /
//    \  2      4  /
[<Class>]
type BipartiteMatching(N,M) =
    let mutable (used : bool []) = [||]
    let tail = N + M + 1
    let mutable graph = Graph(tail + 1)
    

    do // s と t のパスを繋げる
        for i in 1..N do
            graph.Add 0 i
        for i in 1..M do
            graph.Add (i + N) tail


    let rec dfs v = 
        if v = tail then 1  // マッチングが成功
        else
            used.[v] <- true

            let rec check index = 
                if index = graph.graph.[v].Count then 0
                else
                    let e = graph.graph.[v].[index]
                    if not used.[e.To] && e.Cap > 0  //　使われていなくて容量も残っている場合
                    then
                        let d = dfs e.To 
                        if d > 0
                            then
                                //　マッチングが成功していたら　通ったパスを更新
                                graph.graph.[v].Item(index).Cap  <- e.Cap - d
                                graph.RevEdgeCap e <- graph.RevEdgeCap e + d
                                d
                            else check (index + 1)
                    else check (index + 1)
            
            check 0


    member this.maxFlow = 
        let mutable flow = 0
        let rec calc () =
            used <- Array.init (tail + 1) (fun _ -> false)
            let f = dfs 0
            flow <- flow + f
            if f = 0 // 増加パスがなくなったら
            then flow
            else calc ()
        calc ()
    
    member this.Add a b =
        graph.Add (a + 1) (b + N + 1)

// ----- end lib -----

// ----- input -----
let N = stdin.ReadLine() |> int

let G = BipartiteMatching(N,N)

let A = 
    [|
        for i in 1..N -> 
            stdin.ReadLine().Split() 
            |> Array.map int
    |]

let B = 
    [|
        for i in 1..N -> 
            stdin.ReadLine().Split() 
            |> Array.map int
    |]

// ----- end input -----

for i in 0..N-1 do
    let a = A.[i]
    for j in 0..N-1 do
        let b = B.[j]
        if a.[0] < b.[0] && a.[1] < b.[1] then
            G.Add i j

G.maxFlow
|> printfn "%d"