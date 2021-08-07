let N = stdin.ReadLine() |> int
let sorted () = 
    stdin.ReadLine().Split() 
    |> Array.map int 
    |> Array.sort

let A = sorted ()
let B = sorted ()
let C = sorted ()

let rec calc (a:int[]) (b:int[]) ai bi (v : int list) = 
    if ai = a.Length || bi = b.Length then v
    else
        if a.[ai] < b.[bi] then calc a b (ai + 1) (bi + 1) (b.[bi] :: v)
        else calc a b ai (bi+1) v

calc A B 0 0 []
|> fun x -> [|for i in x -> i|]
|> Array.rev
|> fun x -> calc x C 0 0 []
|> fun x -> x.Length
|> printfn "%d"