// 参考　https://qiita.com/drken/items/4a7869c5e304883f539b
//      https://qiita.com/drken/items/a803d4fc4a727e02f7ba

[<Class>]
type DFS (N) = //Nは頂点数
    let mutable Graph = [|for i in 0 .. N-1 -> []|]
    let mutable seen = [|for i in 0 .. N-1 -> false|]
    let rec d v = // 探索
        seen.[v] <- true
        for i in Graph.[v] do  // 配列に含まれるところを探索
            if seen.[i] = false then // 探索済みのところはスルー
                d i


    member this.List = Graph

    member this.AddA2B a b = // 有向　//無効グラフの時はもう１行増やす
        Graph.[a] <- b :: Graph.[a]
         //前にbを入れてるので逆になる

    member this.AddAB a b =  //無向
        this.AddA2B a b
        this.AddA2B b a

    member this.dfs v = 
        seen <- [|for i in 0 .. N-1 -> false|] //初期化
        d v
     
    member this.Reachable s t = 
        seen <- [|for i in 0 .. N-1 -> false|]
        d s
        seen.[t]
    
    member this.length s = // 何個下に繋がっているか
        this.dfs s
        let mutable l = 0
        for i in seen do
            if i = true then l <- l + 1
        l    