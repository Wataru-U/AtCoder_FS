
let [|N;M|] = stdin.ReadLine().Split() |> Array.map int64
if M = 0L
    then printfn "1"
    else
        let mutable m = 10000000000L
        let A = stdin.ReadLine().Split() |> Array.map int64 |> Array.sort
        let dis = [|if A.[(M|> int) - 1] <> N
                    then
                        if A.[0] = 1L
                            then
                                for i in 0 .. ((M|>int)-1) 
                                    -> if i <> (M|>int)-1 then A.[i+1] - A.[i] - 1L else N - A.[i]
                            else
                                for i in 0 .. ((M|>int)) 
                                    -> if i = 0 then A.[0] - 1L else if i <> (M|>int) then A.[i] - A.[i-1] - 1L else N - A.[i-1]
                    else
                        if A.[0] = 1L
                            then
                                for i in 0 .. ((M|>int)-2) 
                                    ->  A.[i+1] - A.[i] - 1L
                            else
                                for i in 0 .. ((M|>int)-1) 
                                    -> if i = 0 then A.[0] - 1L else A.[i] - A.[i-1] - 1L
                    |]
        for i in dis do
            if i <> 0L && m > i
                then m <- i
        
        if m = 10000000000L 
            then 
                printfn "0"
            else
                let mutable c = 0L;
                for i in dis do
                    if i % m = 0L 
                        then c <- c + i / m 
                        else c <- c + i / m + 1L
                printfn "%d" c
                  
