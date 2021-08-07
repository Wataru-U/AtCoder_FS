let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
let A = stdin.ReadLine().Split() |> Array.map int

let mutable num = [|for i in 1..N+1 -> 0|]
let mutable min = 0
let rec culc () =
    min <- min + 1
    if num.[min] > 0 then culc () else ()
let mutable ans = 10000000
for i in 0 .. M-1 do
    num.[A.[i]] <- num.[A.[i]] + 1
if num.[0] <> 0 then culc ()
ans <- min
let reduse x = 
    num.[A.[x]] <- num.[A.[x]] - 1
    if num.[A.[x]] = 0 && min > A.[x]
        then 
            min <- A.[x]
            if min < ans then ans <- min else ()
        else ()
let add x = 
    num.[A.[x]] <- num.[A.[x]] + 1
    if A.[x] = min
        then 
            culc ()
        else ()
let rec answer start tail = 
    if ans = 0 then printfn "0"
    elif tail = N then printfn "%d" ans
    else
        if A.[start] = A.[tail] 
            then answer (start+1) (tail+1)
            else
                reduse start
                add tail
                answer (start+1) (tail+1)
answer 0 M