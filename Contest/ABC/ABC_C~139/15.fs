let [|N;K|] = stdin.ReadLine().Split() |> Array.map int
let T = [|for i in 1..N -> stdin.ReadLine().Split() |> Array.map int|]

let rec ans i v = 
    if i = N then false
    else
        let mutable b = false
        do
            for j in T.[i] do
                b <- b || v ^^^ j = 0
                if not b then
                    b <- b || ans (i+1) (v ^^^ j)
        b
ans 0 0
|> function
    | true -> "Found"
    | false -> "Nothing"
|> printfn "%s"