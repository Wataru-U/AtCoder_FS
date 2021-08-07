let [|N;K|] = stdin.ReadLine().Split() |> Array.map int
let arr = stdin.ReadLine().Split() |> Array.map int64

if K = 1 
then
    arr
    |> Array.map (abs)
    |> Array.min
else
    [|for i in 0..(N - K) ->
        let tail = i + K - 1
        if arr.[i] > 0L && arr.[tail] > 0L || arr.[i] < 0L && arr.[tail] < 0L
        then
            max (abs arr.[i]) (abs arr.[tail])
        else
            (abs arr.[i]) + arr.[tail]
            |> fun x -> x + min (abs arr.[i]) arr.[tail]
    |]
    |> Array.min
|> printfn "%d"