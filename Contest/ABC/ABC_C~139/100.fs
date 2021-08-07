let mutable count = 0

let rec calc x = 
    if x % 2 = 1 then 1
    else
        count <- count + 1
        calc (x/2)

let N = stdin.ReadLine() |> int
stdin.ReadLine().Split() 
|> Array.map int
|> Array.map calc

printfn "%d" count

