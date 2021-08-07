let mutable [|A;B;C|] = stdin.ReadLine().Split() |> Array.map int

if C = 0 then A <- A-1 else B <- B-1

if A <> B
    then if A > B then "Takahashi" else "Aoki"
    else if C = 0 then "Takahashi" else "Aoki"
|> printfn "%s" 