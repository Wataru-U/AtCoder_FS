stdin.ReadLine().ToCharArray()
|> fun x ->
    printfn "%c%c%c" x.[1] x.[2] x.[0]