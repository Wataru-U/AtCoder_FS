let Num2Char v = 
    int('a') - 1 + v
    |> char

stdin.ReadLine().Split()
|> Array.map (int >> Num2Char)
|> fun x -> new string(x)
|> printfn "%s"
