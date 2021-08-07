let xy = stdin.ReadLine().Split(' ') |> Array.map int
let x = (xy.[0] + xy.[1]) / 2
let y = (xy.[0] - xy.[1]) / 2
printfn "%d %d" x y