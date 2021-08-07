let n = stdin.ReadLine() |> int
let s = stdin.ReadLine()

let mutable t = ""
let mutable c = 0

for i in 0 .. (n-1) do 
    c <- c + 1
    t <- t + (s.[i] |> string)
    if c >= 3
        then 
            while (t.[c-3..c-1] = "fox") do
                c <- c-3
                let t_ = t
                t <- ""
                if c > 3 then
                    for j in 0 .. c-4 do
                        t <- t + (t_.[j] |> string)

printfn "%d" t.Length   