let [|H;W|] = stdin.ReadLine().Split() |> Array.map int
let S = 
    [|for i in 1..H -> 
        stdin.ReadLine().ToCharArray()
        |> fun x ->
            [|for j in 1..W -> x.[j-1] = '#'|]
    |]

let check x y = 
    let mutable result = not S.[x].[y]
    do 
        if x > 0 then
            if S.[x-1].[y] then result <- true
        if x < H - 1 then
            if S.[x+1].[y] then result <- true
        if y > 0 then
            if S.[x].[y-1] then result <- true
        if y < W - 1 then
            if S.[x].[y+1] then result <- true
    result

let rec calc x y = 
    if x = H then "Yes"
    elif y = W then calc (x + 1) 0
    else
        if check x y then
            calc x (y + 1)
        else "No"

calc 0 0 
|> printfn "%s"