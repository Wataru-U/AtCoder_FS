let S = stdin.ReadLine().ToCharArray()

let maru = Array.filter (fun x -> x = 'o') S |> fun x -> x.Length

let mutable ans = 0

for i in 0..9 do
    if S.[i] <> 'x' then
    for j in 0..9 do
        if S.[j] <> 'x' then
        for k in 0..9 do
            if S.[k] <> 'x' then
            for l in 0..9 do
                if S.[l] <> 'x' then
                let mutable seen = [|for i in 0..9 -> false|]
                if S.[i] = 'o' then seen.[i] <- true
                if S.[j] = 'o' then seen.[j] <- true
                if S.[k] = 'o' then seen.[k] <- true
                if S.[l] = 'o' then seen.[l] <- true
                seen 
                |> Array.filter (fun x -> x)
                |> Array.length
                |> fun x -> if x = maru then ans <- ans + 1

printfn "%d" ans