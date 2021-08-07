let rec calc v N top ans = 
    if v = top then ans 
    elif N % v = 0L then
        let x = N / v
        calc (v + 1L) N top (min ans (v + x - 2L))
    else calc (v + 1L) N top ans

let N = stdin.ReadLine() |> int64 
let s = N |> float |> sqrt |> int64

calc 1L N (s + 1L) N
|> printfn "%d"
