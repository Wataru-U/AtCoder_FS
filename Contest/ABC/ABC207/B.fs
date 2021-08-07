let [|A;B;C;D|] = stdin.ReadLine().Split() |> Array.map int64
if B >= C * D then -1L
else
    let a = C * D - B
    if A % a = 0L then A / a
    else (A/a) + 1L
|> printfn "%d"