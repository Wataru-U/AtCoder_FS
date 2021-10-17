let s = stdin.ReadLine()
let left = [for i in 0..s.Length-2 -> s.[i+1..] + s.[..i]]

s :: left 
|> List.sort
|> fun x -> 
    printfn "%s\n%s" x.[0] x.[x.Length-1]