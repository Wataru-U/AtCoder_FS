stdin.ReadLine().ToCharArray()
|> Array.map (string >> int)
|> fun x ->
    (x.[0] = x.[1] && x.[0] = x.[2] && x.[0] = x.[3]) ||
    (
        (x.[0] + 1) % 10 = x.[1]
        && (x.[1] + 1) % 10 = x.[2]
        && (x.[2] + 1) % 10 = x.[3]
    )
|> function 
    | true -> "Weak"
    | false -> "Strong"
|> printfn "%s"