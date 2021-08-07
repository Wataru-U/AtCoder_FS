let N = stdin.ReadLine() |> int
let mutable ans = 0
for i in 1 .. N do
    ans <- ans + N / i
    if N % i = 0 then ans <- ans - 1

printfn "%d" ans