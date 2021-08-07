let n = stdin.ReadLine() |> int
let mutable s = stdin.ReadLine()
let mutable s_1 = s.[0..n-1].ToCharArray()
let mutable s_2 = s.[n..].ToCharArray()
let q = stdin.ReadLine() |> int
let swap a b = 
    if b <= n then
        let temp = s_1.[a-1]
        s_1.[a-1] <- s_1.[b-1]
        s_1.[b-1] <- temp
    elif a <= n then
        let temp = s_1.[a-1]
        s_1.[a-1] <- s_2.[b-1-n]
        s_2.[b-1-n] <- temp
    else
        let temp = s_2.[a-1-n]
        s_2.[a-1-n] <- s_2.[b-1-n]
        s_2.[b-1-n] <- temp

let strswap() = 
    let temp = s_1
    s_1 <- s_2
    s_2 <- temp

for i in 1..q do
    let In = stdin.ReadLine().Split() |> Array.map int
    if In.[0] = 1 then
        swap In.[1] In.[2]
    else 
        strswap()

new string(s_1) + new string(s_2 )
|> printfn "%s"