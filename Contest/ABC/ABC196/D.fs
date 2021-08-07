let [|H;W;A;B|] = stdin.ReadLine().Split() |> Array.map int
let mutable ans = 0
let set (grid:bool[][]) x y = 
    grid.[x+1].[y] <- true
    grid
let rec culc a b x y (grid:bool[][]) = 
    printfn "%d %d %b" x y (y >= H)
    let mutable ans = 0
    if x >= W || y >= H || a < 0 || b < 0 
        then 
            0
    elif x = W-1 && y = H-1 && (b = 1 || (a = 0 && b = 0)) then printfn "1"; 1
    elif grid.[x].[y] = true
        then 
        printfn ""
        if y <> H-1
            then 
                (culc a b x (y+1) grid)
        elif x <> W-1
            then
                (culc a b (x+1) 0 grid)
            else 0
        
    elif x <> W-1 //最後の列ではない
        then 
            if y <> H-1
                then //行を変えない
                    ans <- ans + (culc a (b-1) x (y+1) grid)
                    ans <- ans + (culc (a-1) b x (y+1) (set grid x y))
                    if y <> H-2 
                        then 
                            if grid.[x].[y+1] = false
                                then
                                    ans <- ans + (culc (a-1) (b) x (y+2) grid)
                                    ans
                                else 
                                    ans
                        else
                        if grid.[x].[y+1] = false
                            then
                                ans <- ans + (culc (a-1) b (x+1) 0 grid)
                                ans
                            else
                                ans
                else 
                    ans <- ans + (culc a (b-1) (x+1) 0 grid)
                    ans <- ans + (culc (a-1) b (x+1) 0 (set grid x y))
                    ans
        else //最後の行
            ans <- ans + (culc a (b-1) x (y+1) grid)
            if grid.[x].[y+1] = false
                then
                    if y = H-2 then 1
                    else
                        ans <- ans + (culc (a-1) b x (y+2) grid)
                        ans
                else 
                    ans
culc A B 0 0 [|for i in 1..W -> [|for j in 1..H -> false|]|]
|> printfn "%d"
