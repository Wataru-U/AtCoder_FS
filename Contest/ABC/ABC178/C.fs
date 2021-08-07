let N = stdin.ReadLine() |> int
let MOD = 1000000007
let mutable ans:int = 
    if N <= 1 
        then 0 
        else N * (N - 1)
if ans <> 0 then
    for i in 1..N-2 do
        ans <- (ans * 8) % MOD

ans |> printfn "%d"