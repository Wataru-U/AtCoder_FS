let [|a;b|] = stdin.ReadLine().Split() |> Array.map int
let [|c;d|] = stdin.ReadLine().Split() |> Array.map int
let ac = abs (a-c)
let bd = abs (b-d)
let ab = a + b
let cd = c + d
let mab = a-b
let mcd = c - d
if a = c && b = d then 0
elif ac + bd <= 3 || ab = cd || mab = mcd then 1
elif ab % 2 = cd % 2 || ac + bd <= 5 then 2
elif abs(ab - cd) <= 3 || abs(mab - mcd) <= 3 then 2
else 3
|> printfn "%d" 