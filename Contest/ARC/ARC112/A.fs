let [|B;C|] = stdin.ReadLine().Split() |> Array.map int64
let absB = abs B
let mutable ans = 1L
let m = if C < 0L then 1L else 0L
let d_m = (C - m)/2L
if d_m >= absB then ans <- ans + 2L else if C % 2L = 1L then ans <- ans + 1L
let zero = min (absB-1L) d_m
ans <- ans + zero * 2L
let c_ = C - (if B > 0L then 1L else 0L)
if c_ > 2L
    then
    if c_ % 2L = 1L
        then ans <- ans + c_ - 1L
        else ans <- ans + c_ - 1L

ans |> printfn "%d"