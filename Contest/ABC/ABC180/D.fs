let [|X;Y;A;B|] = stdin.ReadLine().Split(' ') |> Array.map int64
let mutable v = X
let mutable count = 0L
while v < B && v * A < Y do
    v <- v * A
    count <- count + 1L
count <- count + if (Y-v) % B = 0L then (Y-v) / B - 1L else (Y-v) / B
printfn "%d" count
