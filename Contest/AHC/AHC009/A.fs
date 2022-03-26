open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text

// bfsで 最短経路をとって 壁際であそびを足す

let rand = new Random()

let [|sx;sy;tx;ty;p|] = stdin.ReadLine().Split() |> Array.map float
let [|si;sj;ti;tj|] = [|sx;sy;tx;ty|] |> Array.map int
let hmap = [| for i in 1..20 -> stdin.ReadLine().ToCharArray() |> Array.map ((=) '1') |]
let vmap = [| for i in 1..19 -> stdin.ReadLine().ToCharArray() |> Array.map ((=)'1')  |]

let map = [|for i in 1..20 -> [|for j in 1..20 -> -1 |]|]
let dir = [|for i in 1..20 -> [|for j in 1..29 -> "."|]|]
let queue = [|for i in 1..400 -> (-1,-1)|]
let mutable tail = 1

let mutable ans = [|for i in 1..200 -> "."|]
let result = new List<string>()

// --------- 関数 ------------
let dirCheck i j d = 
    let [|di;dj|] = 
        match d with 
        | "U" -> [|-1;0|]
        | "D" -> [|1;0|]
        | "L" -> [|0;-1|]
        | _ -> [|0;1|]
    let [|ni;nj|] = [|i + di; j + dj|]
    if ni = -1 || nj = -1 || ni = 20 || nj = 20 then ()
    elif map.[ni].[nj] <> -1 then ()
    else
        let isWall = 
            match d with
            | "U" -> vmap.[i-1].[j]
            | "D" -> vmap.[i].[j]
            | "L" -> hmap.[i].[j-1]
            | _ -> hmap.[i].[j]
        if not isWall 
        then
            map.[ni].[nj] <- map.[i].[j] + 1
            dir.[ni].[nj] <- d
            queue.[tail] <- (ni,nj)
            tail <- tail + 1

let rec bfs i = 
    if i = 400 then ()
    elif map.[ti].[tj] <> -1 then ()
    elif tail = i then ()
    else
        do
            let x = fst queue.[i]
            let y = snd queue.[i]
            //　前と同じ向きを優先
            if dir.[x].[y] <> "."
            then
                dirCheck x y dir.[x].[y]
            dirCheck x y "U"
            dirCheck x y "D"
            dirCheck x y "L"
            dirCheck x y "R"
        bfs (i+1)

// 道をたとって経路の復元
// 復元したものは逆になる
let rec restore i j index = 
    if dir.[i].[j] = "." then ()
    else
        let d = dir.[i].[j]
        let [|di;dj|] = 
            match d with 
            | "D" -> [|-1;0|]
            | "U" -> [|1;0|]
            | "R" -> [|0;-1|]
            | _ -> [|0;1|]
        ans.[index] <- d
        restore (i + di) (j + dj) (index + 1)

// 壁際で曲がるときに マージンを取る 
let rec recruitment i continue prev remain x y = 
    if i = ans.Length then ()
    else
        let [|ni;nj|] = 
            match ans.[i] with 
                | "U" -> [|x-1;y|]
                | "D" -> [|x+1;y|]
                | "L" -> [|x;y-1|]
                | _ -> [|x;y+1|]
        let isWall =  
            match prev with
                | "U" -> if x = 0 then true else vmap.[x-1].[y]
                | "D" -> if x = 19 then true else vmap.[x].[y]
                | "L" -> if y = 0 then true else hmap.[x].[y-1]
                | _ -> if y = 19 then true else hmap.[x].[y]
        let mutable r = remain
        if ni = ti && nj = tj 
        then
            do 
                if prev <> ans.[i] && isWall 
                then
                for j in 1..(max 5 continue) do
                    do
                        if r > 0 then
                            r <- r - 1
                            result.Add prev
            for j in 1..(max 5 continue) do
                do 
                    if r > 0 || j = 1
                    then
                        r <- r - 1
                        result.Add ans.[i]
            if r > 0 then
                for i in 1..r do
                    rand.Next 4
                    |> function
                        | 0 -> "U"
                        | 1 -> "D"
                        | 2 -> "R"
                        | _ -> "L"
                    |> result.Add
        elif prev = ans.[i] 
        then
            result.Add prev
            recruitment (i + 1) (continue + 1) prev remain ni nj
        else
            do
                if i <> 0 && isWall
                then
                for i in 1..(max 3 continue) do
                    do 
                        if r > 0
                        then
                            r <- r - 1
                            result.Add prev
            let nd = ans.[i]
            result.Add nd
            recruitment (i + 1) 1 nd r ni nj
            


// ------- 処理 --------

queue.[0] <- (si,sj)
map.[si].[sj] <- 0

bfs 0

restore ti tj 0

ans <- 
    ans 
    |> Array.filter (fun x -> x <> ".")
    |> Array.rev

recruitment 0 0 "." (200 - ans.Length) si sj

System.String.Concat result
|> printfn "%s"