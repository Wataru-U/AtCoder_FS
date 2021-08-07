let toNum c = 
    match c with
    | 'b' | 'c' | 'B' | 'C' -> '1'
    | 'd' | 'w' | 'D' | 'W' -> '2'
    | 't' | 'j' | 'T' | 'J' -> '3'
    | 'f' | 'q' | 'F' | 'Q' -> '4'
    | 'l' | 'v' | 'L' | 'V' -> '5'
    | 's' | 'x' | 'S' | 'X' -> '6'
    | 'p' | 'm' | 'P' | 'M' -> '7'
    | 'h' | 'k' | 'H' | 'K' -> '8'
    | 'n' | 'g' | 'N' | 'G' -> '9'
    | 'z' | 'r' | 'Z' | 'R' -> '0'
    | _ -> 'a'

stdin.ReadLine()
stdin.ReadLine().Split()
|> Seq.map
    ( fun x ->
        x.ToCharArray()
        |> Array.map toNum
        |> Array.filter 
            (function
                | 'a' -> false
                | _ -> true
            )
    )
|> Seq.filter (fun x -> x.Length <> 0)
|> Seq.map (fun x -> new string(x))
|> fun a -> Seq.iteri (fun i x -> if i <> (Seq.length a) - 1 then printf "%s " x else printf "%s" x) a
printfn ""