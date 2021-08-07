let [|A;B|] = stdin.ReadLine().Split() |> Array.map int
if A <= B && A * 6 >= B then "Yes"
else "No"
|> printfn "%s"