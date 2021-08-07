let N = stdin.ReadLine() |> int
let K = stdin.ReadLine() |> int
let map = [|for i in 1..N -> stdin.ReadLine()|]

let rec calc l u x y dist b (arr:int [] list) = 
    if x < 0 || x = N || y < 0 || y = N then 0
    elif map.[x].[y] = '#' then 0
    elif (u > y || u = y && x <= l) && b then 0
    else
        let mutable a = false
        for i in arr do
            do if i.[0] = x && i.[1] = y then a <- true
        printfn "%A %b %d" arr a dist
        if a 
        then 0
        elif dist = K 
        then  1 
        else
            [|
                calc l u (x + 1) y (dist + 1) true ([|x;y|] :: arr);
                calc l u (x - 1) y (dist + 1) true ([|x;y|] :: arr);
                calc l u x (y + 1) (dist + 1) true ([|x;y|] :: arr);
                calc l u x (y - 1) (dist + 1) true ([|x;y|] :: arr)
            |]
            |> Array.sum

[|
    for i in 0..N-1 ->
        [|
            for j in 0..N-1 ->
                calc i j i j 1 false []
        |]
|]
