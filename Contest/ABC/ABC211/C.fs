let S = stdin.ReadLine()
let MOD = (1e+9 |> int64) + 7L

let toNum x = 
    match x with
    | 'c' -> 0
    | 'h' -> 1
    | 'o' -> 2
    | 'k' -> 3
    | 'u' -> 4
    | 'd' -> 5
    | 'a' -> 6
    | 'i' -> 7
    | _ -> -1

let mutable chokudai = [|for i in 1..8 -> 0L|]

for i in S do
    let n = toNum i
    if n = 0 then
        chokudai.[0] <- chokudai.[0] + 1L
    if n > 0 then
        chokudai.[n] <- (chokudai.[n] + chokudai.[n-1]) % MOD

printfn "%d" chokudai.[7]


