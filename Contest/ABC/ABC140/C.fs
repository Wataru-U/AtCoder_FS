let N = stdin.ReadLine() |> int
let B = stdin.ReadLine().Split() |> Array.map int

let mutable ans = B.[0] + (Array.last B)

for i in 0..N-3 do
    ans <- ans + (min B.[i] B.[i+1])

printfn "%d" ans