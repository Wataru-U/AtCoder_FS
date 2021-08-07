let [|A;B|] = stdin.ReadLine().Split() |> Array.map int
let asum = A * (A+1) / 2
let bsum = B * (B+1) / 2
let diff = asum - bsum |> abs
if diff = 0 then
    for i in 1 .. A do
        printf "%d " i
    for i in 1 .. (B-1) do
        printf "%d " -i
    printfn "%d" -B
elif A < B then
    for i in 1..(A-1) do
        printf "%d " i
    printf "%d " (A+diff)
    for i in 1..(B-1) do
        printf "%d " -i
    printfn "%d" (-B)
else
    for i in 1..A do
        printf "%d " i
    for i in 1..(B-1) do
        printf "%d " -i
    printfn "%d" (-B-diff)
