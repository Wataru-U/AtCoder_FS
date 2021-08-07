let [|x;y|] = stdin.ReadLine().Split() |> Array.map int
if x = y then x
else 3 - x - y
|> stdout.WriteLine
