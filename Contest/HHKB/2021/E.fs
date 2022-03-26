let N = stdin.ReadLine() |> int
let A = [|for i in 1 .. N -> stdin.ReadLine().Split(' ') |> Array.map int |]

let E arr = 
    