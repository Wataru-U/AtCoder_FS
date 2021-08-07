let [|N;M|] = stdin.ReadLine().Split() |> Array.map int

let rec culc (n:int64) m =
    if m = 0 then printfn "%d" n
    elif n % 200L = 0L then culc (n/200L) (m-1)
    else culc (n * 1000L + 200L) (m-1)

culc (N |> int64) M