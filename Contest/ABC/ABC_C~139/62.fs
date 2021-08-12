let [|H;W|] = stdin.ReadLine().Split() |> Array.map int
let area = H * W
let fst x = x |> (fun (a,b,c) -> a)
let snd x = x |> (fun (a,b,c) -> b)
let thd x = x |> (fun (a,b,c) -> c)
let calc (x:int * int * int) = 
    [|
        [|fst x;(snd x) / 2 * (thd x);((snd x) - (snd x) / 2) * (thd x)|] ;
        [|fst x;((snd x) / 2 - 1) * (thd x); ((snd x) - ((snd x) / 2 - 1)) * thd x|] ;
        [|fst x;(thd x) / 2 * (snd x);((thd x) - (thd x) / 2) * snd x|] ;
        [|fst x;((thd x) / 2 - 1) * (snd x); ((thd x) - ((thd x) / 2 - 1)) * snd x |] ;
    |]
    |> Array.map (fun y -> (Array.max y) - (Array.min y))
    |> Array.min

if (H % 3 = 0 || W % 3 = 0)
then 0
else
    [|
        (H / 3 * W,(H - H / 3),W);
        ((H / 3 + 1) * W,(H - H / 3 - 1),W);
        (W / 3 * H,H,(W - W / 3));
        ((W / 3 + 1) * H,H,(W - W / 3 - 1));
    |]
    |> Array.map calc
    |> Array.min
|> printfn "%d"

