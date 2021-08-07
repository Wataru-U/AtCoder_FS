let N = stdin.ReadLine().Split(' ') |> Array.map int 
printfn "%d" (N.[0] - N.[1] + N.[2])