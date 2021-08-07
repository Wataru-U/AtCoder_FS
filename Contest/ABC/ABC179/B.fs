let N = stdin.ReadLine() |> int
let mutable c = 0;
let mutable b = false;
for i in 0..N-1 do
    let In = stdin.ReadLine().Split(' ') |>Array.map int
    if In.[0] = In.[1] 
        then
            c <- c + 1
            if c = 3 then b <- true
        else 
            c <- 0

let ans = if b = true then "Yes" else "No"
ans |> printfn "%s"