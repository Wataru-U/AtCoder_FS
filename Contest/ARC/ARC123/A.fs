let [|A;B;C|] = stdin.ReadLine().Split() |> Array.map int64
let ab = B - A
let bc = C - B

if ab = bc then 0L
elif A > C then
    if ab < bc then
        if (bc - ab) % 2L = 0L then (bc - ab) / 2L
        else (bc - ab + 1L) / 2L + 1L
    else 
        B - A + B - C
else
    if (C + A) / 2L > B then
        if (C + A) % 2L = 1L then (C + A + 1L) / 2L - B + 1L
        else (C + A) / 2L - B 
    else
        ab - bc
|> printfn "%d"



stdin.ReadLine().Split()
|> Array.map int64
|> fun x -> 2L * x.[1] - x.[0] - x.[2]
|> fun x ->
    if x < 0L then
        if x % 2L = 0L then - x / 2L
        else - (x-1L) / 2L + 1L
    else x
|> printfn "%d"