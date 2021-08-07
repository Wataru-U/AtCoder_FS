let n = stdin.ReadLine() |> int
let A = stdin.ReadLine().Split() |> Array.map int64 |> Array.sort
let MOD = 1000000007L
let rec diff idx Base = 
    if idx = -1 then 0L
    elif A.[idx] = Base 
    then diff (idx - 1) Base
    else
        Base - A.[idx]
let rec culc (ans:int64) (idx:int) =
    if idx = n then ans
    elif idx = 0 then 
        let d = diff (idx-1) A.[idx]
        culc (ans + ans * d % MOD) (idx+1)
    elif A.[idx] = A.[idx-1] then culc ans (idx+1)
    else
        let d = diff (idx-1) A.[idx]
        culc (ans * (d+1L) % MOD) (idx+1)
if n = 1 then printfn "%d" (A.[0] + 1L)
else culc (A.[0]+1L) 1 |> printfn "%d"
