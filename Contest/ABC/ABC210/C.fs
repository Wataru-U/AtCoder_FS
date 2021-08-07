let [|N;K|] = stdin.ReadLine().Split() |> Array.map int
let c = stdin.ReadLine().Split() |> Array.map int

// ----- 事前準備　------


//　c < 10^9　なので　[|0..c.max|] は不可
//　しかし N < 3 * 10^5 なので　出てくる色の種類は高々 N
//　c の重複を消してソート　で種類分けの準備
let mutable num = 
    c
    |> Array.countBy id
    |> Array.map (fun (x,_) -> x)
    |> Array.sort

let l = num.Length - 1

//　２分探索して　インデックスに変えることで　c.max < N に変換
let rec tansaku v top bottom = 
    if top - bottom = 1 then
        if num.[bottom] = v then bottom else top
    else
        let index = (top + bottom) / 2
        if num.[index] = v then index
        elif num.[index] < v then tansaku v top index
        else tansaku v index bottom

// c の各要素を２分探索で変換
let arr =
    c
    |> Array.map (fun x -> tansaku x l 0)

// c.max < 3 * 10^5 になったので　[|0..c.max|] が可能に
let mutable b = [|for i in 0..l -> 0|]


// ----- 処理　------


// 各　K個の区間で　新しく入る色と出ていく色の処理

let mutable count = 0
for i in 0..K-1 do
    b.[arr.[i]] <- b.[arr.[i]] + 1
    if b.[arr.[i]] = 1 
    then
        count <- count + 1

let mutable ans = count
for i in K .. N-1 do
    b.[arr.[i]] <- b.[arr.[i]] + 1
    if b.[arr.[i]] = 1
    then
        count <- count + 1
    
    let idx = i - K
    b.[arr.[idx]] <- b.[arr.[idx]] - 1
    if b.[arr.[idx]] = 0 then
        count <- count - 1
    ans <- max ans count

printfn "%d" ans


