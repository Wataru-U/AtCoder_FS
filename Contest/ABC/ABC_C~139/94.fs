let N = stdin.ReadLine() |> int
let X = 
    stdin.ReadLine().Split() 
    |> Array.map int

let sorted = X |> Array.sort    

let center = N / 2 - 1 |> max 0

let center_l = sorted.[center]
let center_u = sorted.[center + 1]

for i in X do
    if center_l = center_u then center_l
    elif i <= center_l then center_u 
    else center_l
    |> printfn "%d"