//セグ木の引数に関数を入れていろいろなケースに対応
//引数を2つ要求する関数を入れる
//要素になんかするときの配列番号の範囲　0~N-1
//クエリ処理をするときの配列番号の範囲　1~N

let solve a b = a ^^^ b

[<Class>]
type SegmentTree(N:int,fanc) = 
    let rec pow b = if b = 0 then 1 else 2 * pow (b-1) 
    let rec l2 v b c = if v < b * 2 then c else l2 v (b*2) c+1

    let h = l2 N 1 1

    let length = pow h * 2 //葉の数 

    let mutable sum = [| for i in 1 .. length -> 0L |]

    //上にf
    let rec reculc n (v:int64) l = 
        if n <> 0 then 
            if(n % 2 = 1)
                then
                    sum.[(n-1)/2] <- fanc sum.[n]  sum.[n+1]
                else
                    sum.[(n-1)/2] <- fanc sum.[n]  sum.[n-1]
            reculc ((n-1)/2) sum.[(n-1)/2] l

    let rec query a b k l r = 
        if r < a || b < l then //範囲外
                0L
            else if k > (length-1) / 2
                then sum.[k]
            else if a <= l && r <= b//範囲内 
                then sum.[k]
                       
            else 
                let vl = query a b (k*2+1) l ((l+r)/2)
                let vr = query a b (k*2+2) ((l+r)/2+1) r
                fanc vr vl
   
    let child n = n + length / 2 - 1 //一番下の層に合わせる
    
    member this.Set n v = 
        sum.[child n] <- v
        reculc (child n) v 0L


    member this.Add n (v:int64) = 
        let l = sum.[child n]
        sum.[child n] <- fanc sum.[child n] v
        reculc (child n) v l

    member this.Query a b = query a b 0 1 (length/2)
    member this.sumArr = sum


let [|N; Q|] = stdin.ReadLine().Split(' ') |> Array.map int
let Seg = SegmentTree(N,solve)
let Arr = stdin.ReadLine().Split(' ') |> Array.map int64
for i in 0 .. N-1 do
    Seg.Set i Arr.[i]
//printfn "%A" Seg.sumArr
for i in 1 .. Q do
    let query =  stdin.ReadLine().Split(' ') |> Array.map int
    if query.[0] = 1 then
        let v = query.[2]|> int64
        Seg.Add (query.[1]-1) v
        else
        printfn "%d" (Seg.Query (query.[1]) (query.[2]))
    //printfn "%A" Seg.sumArr

