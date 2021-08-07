let [|H;W|] = stdin.ReadLine().Split() |> Array.map int
let v = [|for i in 1 .. H -> stdin.ReadLine().ToCharArray()|]

let rec check x y sum b v H W= 
    if x = W && y = W then

