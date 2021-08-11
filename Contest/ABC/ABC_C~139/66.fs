let N = stdin.ReadLine() |> int
let a = stdin.ReadLine().Split() |> Array.map int
if N = 1 then printfn "%d" a.[0]
else
for i in 1..2..N do
    printf "%d " (a.[N - i])
let srt = if N % 2 = 0 then 0 else 1

for i in srt..2..N-3 do
    printf "%d " (a.[i])
printfn "%d" a.[N-2]
