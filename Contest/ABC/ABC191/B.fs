let [|N;X|] = stdin.ReadLine().Split()
stdin.ReadLine().Split() 
|> Array.filter (fun x -> x <> X)
|> fun x -> if x.Length = 0 
                then printfn ""
                else
                    for i in 0 .. x.Length-2 do
                        printf "%s " x.[i]
                    printfn "%s" x.[x.Length-1]
