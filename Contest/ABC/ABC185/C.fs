let N = stdin.ReadLine() |> int64
let mutable ans = N-1L
for i in 1L ..10L do
    ans <- ans * (N - i-1L) / (i+1L)

printfn "%d" ans
