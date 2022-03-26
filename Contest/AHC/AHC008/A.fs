open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text

new StreamWriter(Console.OpenStandardOutput())
|> fun x -> x.AutoFlush <- false; x
|> Console.SetOut


// 回答
let mutable ans = ""

// -------- input ------------
let N = stdin.ReadLine() |> int

let mutable pets = 
    [|
        for i in 1..N ->
            stdin.ReadLine().Split()
            |> Array.map int
            |> Array.map (fun x -> x-1)
    |]
let M = stdin.ReadLine() |> int
let mutable positions = 
    [|
        for i in 1..M ->
            stdin.ReadLine().Split() 
            |> Array.map int
            |> Array.map (fun x -> x-1)
    |]


// -------- 事前処理 ------------

let mutable map = [|for i in 1..30 -> [|for i in 1..30 -> -1 |]|]

// エリアにいるペットの数
let mutable petNums = [|for i in 0..4 -> 0|]
let mutable seted = [| for i in 0..6 -> false|]
seted.[0] <- true
seted.[6] <- true
let rec countPets i x = 
    if i * 5 + 5 > x then petNums.[i] <- petNums.[i] + 1
    else countPets (i+1) x

// 位置の設定
// 動物　<- id, 人　<- id + 100, 通行止め <- 50
let reposition () =
    for i in 0..29 do 
        for j in 0..29 do
            map.[i].[j] <-
                if map.[i].[j] = 50 then 50 else -1
    // 同じマスに複数隊いる場合はペット優先
    for i in 0 .. M-1 do
        let p = positions.[i]
        map.[p.[0]].[p.[1]] <- i + 100  

    petNums <- [|for i in 0..5 -> 0|]
    for i in 0 .. N-1 do 
        let pet = pets.[i]
        map.[pet.[0]].[pet.[1]] <- i
        countPets 0 pet.[0]


let petMotion = Map<char, int * int> [| ('U', (-1,0)); ('D', (1,0)); ('L', (0,-1)); ('R', (0,1));('.',(0,0)); |]
let petAction i v = 
    let move = petMotion.[v]
    pets.[i] <- [|pets.[i].[0] + fst move; pets.[i].[1] + snd move; pets.[i].[2]|]
let rec petMove i (v:string) j = 
    if j = v.Length then ()
    else
        petAction i v.[j]
        petMove i v (j+1)


// ペットがいるかどうか
let isPet x y = 
    if x < 0 || x >= 30 || y < 0 || y >= 30 then true
    else  map.[x].[y] = -1 || map.[x].[y] >= 40
// そのマスに通行止めを置けるかどうか
let sarch x y = 
    if x < 0 || x >= 30 || y < 0 || y >= 30 then false
    else map.[x].[y] = -1 && isPet (x+1) y && isPet (x-1) y && isPet x (y+1) && isPet x (y-1) 
let rec stop_2_4 x y c i j = 
    if i = 2 then c > 1
    elif j = 2 then stop_2_4 x y c (i+1) 0
    elif map.[x - i].[y - j] = 50 then stop_2_4 x y (c + 1) i (j + 1)
    else stop_2_4 x y c i (j + 1)

let mutable seen = [|for i in 1..30 -> [|for j in 1.. 30 -> '.' |]|]
let mutable dist = [|for i in 1.. 30 -> [|for j in 1..30 -> 0|]|]

let mutable queue = [|for i in 1..900 -> ([|0;0;0|],'.')|]
let mutable queueIndex = 0

let rec bfs i = 
    let set sx sy sc sd = 
        if sx < 0 || sx >= 30 || sy < 0 || sy >= 30 then ()
        elif map.[sx].[sy] = 50 || seen.[sx].[sy] <> '.' then ()
        elif queueIndex = 900 then ()
        else
            queue.[queueIndex] <- ([|sx;sy;sd|],sc)
            queueIndex <- queueIndex + 1
            seen.[sx].[sy] <- sc
            dist.[sx].[sy] <- sd
    if i = queueIndex then ()
    else 
        let q = queue.[i]
        let p = fst q
        let c = snd q
        let [|sx;sy;d|] = [|p.[0];p.[1];p.[2]|]
        set (sx - 1) sy 'D' (d + 1)
        set (sx + 1) sy 'U' (d + 1)
        set sx (sy - 1) 'R' (d + 1)
        set sx (sy + 1) 'L' (d + 1)
        bfs (i+1)

let BFS i =
    // 初期化
    seen <- [|for j in 1..30 -> [|for k in 1 .. 30 -> '.' |]|]
    dist <- [|for j in 1..30 -> [|for k in 1 .. 30 -> 0 |]|]
    queue <- [|for i in 1..900 -> ([|0;0;0|],'.')|]
    queueIndex <- 1

    let p = positions.[i]
    let x = p.[0]
    let y = p.[1]

    queue.[0] <- ([|x;y;0|],'S')
    seen.[x].[y] <- 'S'
    dist.[x].[y] <- 0

    bfs 0

let isStop x y = 
    map.[x].[y] <> 50

let rec tadoru i x y (direction:char) = 
    if x < 0 || x >= 30 || y < 0 || y >= 30 then "."
    elif seen.[x].[y] = '.' then "." 
    elif seen.[x].[y] = 'S' 
    then 
        let v = 
            match direction with
                | 'U' -> "D"
                | 'D' -> "U"
                | 'L' -> "R"
                | _ -> "L"
        let d = petMotion.[direction]
        let [|nx;ny|] = [|positions.[i].[0] - fst d; positions.[i].[1] - snd d|]
        do
            if map.[nx].[ny] = -1 || map.[nx].[ny] >= 100 then map.[nx].[ny] <- i
        positions.[i] <- [|positions.[i].[0] - fst d; positions.[i].[1] - snd d|]
        v
    else 
        let nd = seen.[x].[y]
        let d = petMotion.[nd] 
        tadoru i (x + fst d) (y + snd d) nd


// 個人の動き
// 石を置く
let setStopPoint i (d:string) x y = 
    if sarch x y 
    then 
        map.[x].[y] <- 50
        ans <- ans + d
    else 
        ans <- ans + "."

//  個人個人の段階
let Modes = [|for i in 1..M -> 0|]
// Mode = 0
// 決められた場所に移動する
let Step0 i t = 
    let line = i % 5
    let p = positions.[i]
    let [|x;y|] = [|p.[0]; p.[1]|]
    let [|gx;gy|] = [|5 * line + 5;0|]
    if x = gx && y = gy 
    then
        ans <- ans + "."
        Modes.[i] <- 1
    elif t > 80 then ans <- ans + "."
    else 
        BFS i
        ans <- ans + tadoru i gx gy seen.[gx].[gy]

// 石を置いていく
let Step1 i t = 
    let step0s = Modes |> Array.filter (fun x -> x = 0) |> (fun x -> x.Length)
    if t < 60 && step0s <> 0 then ans <- ans + "."
    elif t > 200
    then 
        Modes.[i] <- 2
        ans <- ans + "."
    else
        let line = i % 5
        let p = positions.[i]
        let [|x;y|] = [|p.[0]; p.[1]|]
        if y = 0 
        then
            if map.[x-1].[y] = 50 
            then
                ans <- ans + "R"
                positions.[i] <- [|x; y + 1|]
            elif sarch (x-1) y then setStopPoint i "u" (x-1) y
            else ans <- ans + "."
        elif (map.[x].[y-1] = 50 || map.[x-1].[y] = 50) && (y = 29 || stop_2_4 x y 0 0 0 || map.[x].[y - 1] = 50)
            then 
                if y < 29 
                then 
                    ans <- ans + "R"
                    positions.[i] <- [|x; y + 1|]
                else
                    ans <- ans + "."
                    seted.[line + 1] <- true
                    Modes.[i] <- 2
        elif y < 29 && sarch (x-1) y then setStopPoint i "u" (x-1) y
        elif sarch x (y - 1) then setStopPoint i "l" x (y-1)
        else ans <- ans + "."

// 動物が少ないところに移動
let Step2 i t = 
    let p = positions.[i]
    let [|x;y|] = [|p.[0];p.[1]|]
    let line = i % 5
    let g = 
            petNums
            |> Array.mapi (fun i z -> (i,z))
            |> Array.filter (fst >> fun z -> seted.[z] && seted.[z + 1])
    if g.Length = 0 && t < 281 then ans <- ans + "."
    elif t > 280 
    then 
        Modes.[i] <- 4
        ans <- ans + "."
    else
    let [|gx;gy|] = 
        g
        |> Array.sortBy snd
        |> Array.head
        |> fst
        |> (*) 5
        |> (+) (if i % 2 = 0 then 0 else 5)
        |> min 29
        |> fun z -> [| z;29|]
    if x = gx && y = gy
    then
        Modes.[i] <- 3
        ans <- ans + "."
    else
        BFS i
        ans <- ans + tadoru i gx gy seen.[gx].[gy]

// 道を閉じる
let Step3 i t = 
    if t < 260 then ans <- ans + "."
    else
    let p = positions.[i]
    let [|x;y|] = [|p.[0];p.[1]|]
    let d = if i % 2 = 0 then -1 else 1
    let c = if i % 2 = 0 then "u" else "d"
    if sarch (x + d) y then setStopPoint i c (x + d) (y)
    else ans <- ans + "."

let Step4 i t = 
    let p = positions.[i]
    let [|x;y|] = [|p.[0];p.[1]|]
    let d = -1
    let c = "u"
    if sarch (x + d) y then setStopPoint i c (x + d) (y)
    else ans <- ans + "."

// 行動
let Move i t = 
    match Modes.[i] with
    | 0 -> Step0 i t
    | 1 -> Step1 i t
    | 2 -> Step2 i t
    | 3 -> Step3 i t
    | 4 -> Step4 i t

let Turn t = 
    ans <- ""
    for i in 0..M-1 do
        Move i t
    printfn "%s" ans
    stdout.Flush ()

// -------- メイン処理 ------------

reposition ()

for i in 1 .. 300 do
    Turn i
    // ペットの動きを受け取る
    stdin.ReadLine().Split()
    |> fun x -> 
        for j in 0..N-1 do
            petMove j x.[j] 0
    reposition ()

