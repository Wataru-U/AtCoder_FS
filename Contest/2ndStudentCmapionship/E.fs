let [|A;B|] = stdin.ReadLine().Split() |> Array.map int
let mutable ans = 1 
for i in 2..B do
    if A < i && i * 2 <= B then ans <- i
    else
        if A % i = 0 && A / i < B /i then ans <- i
        if A % i <> 0 && (A / i + 1) < B / i then ans <- i

printfn "%d" ans