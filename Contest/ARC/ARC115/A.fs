let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
let students = [|for i in 0..N-1 -> stdin.ReadLine() |]

let rec same (a:string) (b:string) count difference = 
    if count = M then difference % 2 = 1
    elif a.[count] = b.[count] then (same a b (count+1) difference) else (same a b (count+1) (difference+1))
let mutable ans = 0
for i in 0..N-2 do
    for j in 0..N-1 do
        if same students.[i] students.[j] 0 0
            then ans <- ans + 1