// 参考 https://qiita.com/drken/items/996d80bcae64649a6580

[<Class>]
 type BFS(N) = //Nは頂点数
    let mutable Graph = [|for i in 0..N-1 -> []|]
    let mutable dist = [|for i in 0 .. N-1 -> N|]
    let mutable Queue = [|for i in 0..N -> N|]
    let mutable QueueLength = 0
    let mutable QueueNum = 0 
    let Swap a b = 
        let q = Queue.[a]
        Queue.[a] <- Queue.[b]
        Queue.[b] <- q
        
    let push a b = //まだ回っていない場合キューに追加
        if dist.[a] = N then
            Queue.[QueueLength] <- a
            QueueLength <- QueueLength + 1
            dist.[a] <- dist.[b] + 1
            
    let rec b s =
        if Queue.[QueueNum] <> N then
            QueueNum <- QueueNum + 1
            for i in Graph.[s] do
                push i s
            b Queue.[QueueNum]

    member this.Dist = dist
    member this.AddA2B a b = // 有向　//無効グラフの時はもう１行増やす
        Graph.[a] <- b :: Graph.[a]
         //前にbを入れてるので逆になる

    member this.AddAB a b =  //無向
        this.AddA2B a b
        this.AddA2B b a
    
    member this.bfs s =
        dist <- [|for i in 0 .. N-1 -> N|] 
        Queue <- [|for i in 0..N -> N|]
        dist.[s] <- 0
        for i in Graph.[s] do
            push i s
        b Queue.[0]

let exStdIn = new System.IO.StreamReader( "test.txt" );
System.Console.SetIn( exStdIn );

let [|N;M|] = stdin.ReadLine().Split(' ') |> Array.map int
let b = BFS(N)
for i in 1..M do
    let [|p;c|] =stdin.ReadLine().Split(' ') |> Array.map int |> Array.map (fun a -> a-1)
    b.AddA2B p c
b.bfs 0
for i in b.Dist do
    printfn "%d" i
System.Console.Out.Flush()