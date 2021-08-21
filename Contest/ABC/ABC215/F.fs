// sort すればできそうと思ったけどそれ以上が出てこなかった。
let N = stdin.ReadLine() |> int
let pos = 
    [|
        for i in 1..N ->
            stdin.ReadLine().Split()
            |> Array.map int
    |]

let sortted_x = pos |> Array.sortBy (fun x -> x.[0])
let sortted_y = pos |> Array.sortBy (fun x -> x.[1])