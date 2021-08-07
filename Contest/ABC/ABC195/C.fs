let N = stdin.ReadLine() |> int64
let mutable ans = 0L
let rec culc v e =
    if v * 1000L > N then ans <- ans + e * (N - v + 1L)
    else
        ans <- ans + e * (v * 1000L - v) 
        culc (v * 1000L) (e+1L)   
if N >= 1000L then culc 1000L 1L
printfn "%d" ans