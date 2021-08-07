let N = stdin.ReadLine() |> int64
let arr = 
    [|for i in 1 .. ((sqrt (N|> float) |> int) + 2) ->  if N % (i|> int64) = 0L then true else false |]
for i in 0..arr.Length-1 do
    if arr.[i] = true then printfn "%d" (i+1)
for i in 1..arr.Length do
    let l = arr.Length - i 
    let l64 = l |> int64
    if arr.[l] = true && (arr.Length|>int64) < N/(l64+1L)  then printfn "%d" (N/(l64+1L))    