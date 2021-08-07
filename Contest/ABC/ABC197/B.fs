let [|H;W;X;Y|] = stdin.ReadLine().Split() |> Array.map int
let grid = [|for i in 1..H -> stdin.ReadLine().ToCharArray()|]
let xPos = X-1
let yPos = Y-1
let rec sarch x y dir n = 
    if (x < 0 || y < 0 || x > H-1 || y > W-1) then n
    elif grid.[x].[y] = '#' then n
    elif dir = 0 then sarch x (y-1) dir (n+1)
    elif dir = 1 then sarch (x+1) y dir (n+1)
    elif dir = 2 then sarch x (y+1) dir (n+1)
    else sarch (x-1) y dir (n+1)


if grid.[xPos].[yPos] = '.' 
    then
        1 + (sarch xPos (yPos-1) 0 0) + (sarch (xPos+1) yPos 1 0) + (sarch (xPos) (yPos+1) 2 0) + (sarch (xPos-1) yPos 3 0)
    else 0
|> printfn "%d"