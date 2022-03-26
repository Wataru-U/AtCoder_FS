let [|N;Q|] = stdin.ReadLine().Split() |> Array.map int
let a = stdin.ReadLine().Split() |> Array.map int

let query = [|for i in 1..Q -> stdin.ReadLine().Split() |> Array.map int|]
let s = set [|for i in query -> i.[0]|] |> Array
let q = [|for i in 1..Q -> s.[i-1]|]