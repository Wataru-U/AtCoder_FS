let [|N;A;X;Y|] = stdin.ReadLine().Split() |> Array.map int
if N <= A then N * X
else A * X + (N - A) * Y
|> printfn "%d"