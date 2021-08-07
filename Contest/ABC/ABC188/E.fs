let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
//来るための道のリスト
//0は無視する
let mutable array = [|for i in 0 .. N -> [0] |]
//そこまでの利益の最大値　マイナスの場合があるので初期値は低く
let mutable Max = [|for i in 0 .. N-1 -> -1000000000L |]
let v = stdin.ReadLine().Split() |> Array.map int64
//そこまでの最安値　買う用
let mutable Min = Array.copy v
for i in 1 .. M do
    stdin.ReadLine().Split() |> Array.map int 
    |> fun x ->  array.[x.[1]] <- x.[0] :: array.[x.[1]] //道の先のリストに開始点を入れる

let mutable ans = -10000000000L

let min (a:int64) (b:int64) = if a > b then b else a
let max  (a:int64) (b:int64) = if a > b then a else b


// 動的計画法
for i in 1..N do
    if array.[i].Length <> 1 //来れる道があるなら
        then 
            for j in array.[i] do
                if j <> 0  //初期値0はむしする
                    then
                        //どこで買えば一番安いか　
                        Min.[i-1] <- min Min.[i-1] Min.[j-1]
                        //この街までの利益の最大値
                        Max.[i-1] <- max Max.[j-1] <| max Max.[i-1] (v.[i-1]-Min.[j-1])
                        //全体の利益の最大値
                        ans <- max Max.[i-1] ans

Array.max Max
|> printfn "%d"