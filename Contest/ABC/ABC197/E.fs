let n = stdin.ReadLine() |> int 
let mutable ball = [|for i in 1..n -> []|]
let rec In num = 
    if num = n then ()
    else 
        stdin.ReadLine().Split() 
        |> fun x -> 
            ball.[(x.[1]|>int)-1] <- (x.[0] |> int64) :: ball.[(x.[1]|>int)-1]
        In (num+1)

In 0

ball <- 
    ball
    |> Array.filter (fun x -> x.Length <> 0)
    |> Array.map (fun x -> [List.min x;List.max x])
let l = ball.Length
let rec culc num x ans =
    if num = l then (abs x) + ans
    elif ball.[num].[0] = ball.[num].[1] then culc (num+1) ball.[num].[0] ((abs (x - ball.[num].[0])) + ans) 
    else
        let 1 = ball.[num].Length - 1
        let right = 
            ball.[num].[0] - x 
            |> abs
        let left = 
            ball.[num].[1] - x 
            |> abs
            
        let r = culc (num+1) ball.[num].[0] (if (ball.[num].[1] - x) > 0L then (ans + left * 2L + right) else (ans + right))
        let l = culc (num+1) ball.[num].[1] (if (ball.[num].[0] - x) < 0L then (ans + right * 2L + left) else (ans + left))
        min l r

culc 0 0L 0L
|> printfn "%d"