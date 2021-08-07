// union find tree

let N = stdin.ReadLine() |> int
let A = stdin.ReadLine().Split() |> Array.map int

let mutable parent = [|for i in 0..200000 -> i|]
let rec root x = 
    if x = parent.[x] then x else root parent.[x]

let mutable ans = 0

if N <> 1 
then
  let fHalf = A.[..N/2-1]
  let sHalf = A.[N-N/2..] |> Array.rev

  for i in 0..N/2-1 do
      let fi = root fHalf.[i]
      let si = root sHalf.[i]
      if fi <> si
      then
          ans <- ans + 1
          let idx = min si fi
          parent.[si] <- idx
          parent.[fi] <- idx
          parent.[fHalf.[i]] <- idx
          parent.[sHalf.[i]] <- idx

printfn "%d" ans