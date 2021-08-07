let T = stdin.ReadLine()|> int
for i in 1 .. T do
    let [|L;R|] = stdin.ReadLine().Split() |> Array.map int
    let b = (R - 2 * L + 1) |> int64
    let mutable ans = if b > 0L then (b * (b+1L) / 2L) else 0L
    printfn "%d" ans
