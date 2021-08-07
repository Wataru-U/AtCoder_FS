let [|W;H;x;y|] = stdin.ReadLine().Split() |> Array.map float

let sq = W * H / 2.
let ans = if x = W / 2. && y = H / 2. then 1 else 0
printfn "%15f %d" sq ans