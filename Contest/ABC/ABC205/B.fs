let N = stdin.ReadLine() |> int
let mutable A = [|for i in 1..N -> false|]

stdin.ReadLine().Split()
|> Array.map int
|> Array.map (fun x ->  A.[(x-1)] <- true)

A 
|> Array.filter (fun x -> x = false)
|> fun x -> x.Length
|> function
    | 0 -> "Yes"
    | _ -> "No"
|> printfn "%s"