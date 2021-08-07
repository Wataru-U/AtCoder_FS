let s = stdin.ReadLine()
let N = stdin.ReadLine().Split(' ') |> Array.map int64

N |> Array.map (fun x -> abs x) |> Array.sum |> printfn "%d"
N |> Array.map (fun x -> x * x) |> Array.sum |> double |> sqrt |> string |>  printfn "%s"
N  |> Array.map (fun x -> abs x)|> Array.max |> printfn "%d"