let rec culc (x:int64[]) = 
    if x.Length = 2 
        then min x.[0] x.[1]
        else
            culc [| for i in 0 .. (x.Length/2-1) -> max x.[2 * i] x.[2*i+1]|]

let rec find (v:int64) (x:int64[]) index =
    if x.[index] = v then index + 1
    else find v x (index+1)

let n = stdin.ReadLine() |> int
let v = stdin.ReadLine().Split() |> Array.map int64

culc v
|> fun x -> find x v 0
|> printfn "%d"


stdin.ReadLine()
|> fun x -> stdin.ReadLine().Split() 
|> Array.map int64
|> fun x ->(x, min (Array.max (x.[..(x.Length/2-1)])) (Array.max (x.[(x.Length/2)..])))
|> fun (x,y) -> Array.findIndex (fun elm -> elm = y) x
|> fun x -> x+1
|> printfn "%d"