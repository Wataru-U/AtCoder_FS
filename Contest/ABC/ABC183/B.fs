let [|sx;sy;gx;gy|] = stdin.ReadLine().Split() |> Array.map float
sx + sy * (gx-sx) / (gy + sy) |> string |> printfn "%s"