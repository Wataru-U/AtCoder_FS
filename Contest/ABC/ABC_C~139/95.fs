let [|A;B;C;X;Y|] =
    stdin.ReadLine().Split()
    |> Array.map int

let Ax = A * X 
let Bx = B * Y
let Cx = (max X Y) * C * 2
let Cx_ = 
    min X Y
    |> fun x -> x * C * 2 + (X - x) * A + (Y - x) * B

Ax + Bx
|> min Cx
|> min Cx_
|> printfn "%d"
