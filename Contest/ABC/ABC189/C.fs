let n = stdin.ReadLine() |> int
let A = stdin.ReadLine().Split() |> Array.map int64
let mutable ans = 0L

for i in 0.. (n - 2) do
    let mutable Min = A.[i]
    ans <- max ans A.[i]
    for j in i+1 .. (n-1) do
        Min <- min Min A.[j]
        ans <- max ans (Min * ((j-i + 1)|>int64))

ans <- max ans A.[n-1]

printfn "%d" ans



