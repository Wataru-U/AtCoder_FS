let S = 
    stdin.ReadLine().ToCharArray()
    |> Array.map string
    |> Array.map int
let mutable l = 0

for i in 0..S.Length-1 do
    if S.[i] % 2 = i % 2
    then
        l <- l + 1

min l (S.Length - l)
|> printfn "%d"