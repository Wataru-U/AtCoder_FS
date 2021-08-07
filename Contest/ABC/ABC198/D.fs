let n = stdin.ReadLine() |> int
let col = stdin.ReadLine().Split() |> Array.map int |> Array.map (fun x -> x-1)
let mutable seen = [|for i in 1 .. n -> false|]
let mutable (route:list<int>[]) = [|for i in 1..n -> []|]
let mutable child = [|for i in 1..n -> []|]
let mutable queue = [0]
let add x y = 
    route.[x] <- y :: route.[x]
    route.[y] <- x :: route.[y]

for i in 1..(n-1) do
    stdin.ReadLine().Split() 
    |> Array.map int 
    |> Array.map (fun x -> x-1)
    |> fun x -> add x.[0] x.[1]

let idx num = queue.Length - num - 1

let rec bfs num = 
    if num = queue.Length then ()
    else
        for i in route.[queue.[idx num]] do
            if seen.[i] = false
                then    
                    seen.[i] <- true
                    queue <- i :: queue
                    child.[num] <- i :: child.[num]
        bfs (num + 1)
seen.[0] <- true

let mutable (answer:list<int>) = []

let rec ans num (color:bool[]) = 
    if color.[col.[num]] = false 
        then 
            answer <- (num+1) :: answer
            let mutable c = color
            c.[col.[num]] <- true
           
            for i in  [|for j in 1..child.[num].Length -> child.[num].Item(j-1)|] do
                ans i c
        else
            for i in  [|for j in 1..child.[num].Length -> child.[num].Item(j-1)|] do
                ans i color

bfs 0

ans 0 [|for i in 0..100000 -> false|]



for i in (List.sort answer) do
    printfn "%d" i