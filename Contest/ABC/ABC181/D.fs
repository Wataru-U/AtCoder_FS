//下三桁が8の倍数かどうか調べる

// 0~9の数字がどれだけ入っているかのクラス
[<Class>]
type a(v:string) =
    let mutable n = [|for i in 0..9 -> 0|]
    
    do
        for i in v do
            let j = i |> string |> int
            n.[j] <- n.[j] + 1

    member this.N = n

    // 集合　this　が　b の部分集合かどうか(?)
    member this.compair (b:a) = 
        let mutable bo = true
        for i in 0..9 do
            if n.[i] > b.N.[i] then bo <- false
        bo

// 0 ~ 999　までの　８の倍数は 8 * (1 ~ 124)
let k = [|for i in 1 .. 124 -> a ((8*i)|> string)|]

let str = stdin.ReadLine()

let v = a(str)

let mutable boo = false
//桁に合わせて範囲を変える
if str.Length = 1 then if k.[0].compair v = true then boo <- true
else if str.Length = 2
then
    for i in 1 ..11 do
        if k.[i].compair v = true then boo <- true
else
    for i in 12 .. 123 do
        if k.[i].compair v = true then boo <- true

if boo = true then printfn "Yes" else printfn "No"
     