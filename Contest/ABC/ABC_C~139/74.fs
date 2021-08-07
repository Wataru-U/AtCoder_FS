let [|A;B;C;D;E;F|] = stdin.ReadLine().Split() |> Array.map int

let Amax = F / (100 * A)
let Bmax = F / (100 * B)

let mutable ans = (0,0)
let mutable per = -1.

for i in 0..Amax do
    for j in 0..Bmax do
        let water = (i * A + j * B) * 100
        if water <= F 
        then
            let Cmax = water * E / C / 100
            let Dmax = water * E / D / 100
            for c in 0..Cmax do
                for d in 0..Dmax do
                    let sugar = c * C + d * D
                    if water + sugar <= F && water / 100 * E >= sugar
                    then    
                        if (sugar |> float) / ((water + sugar) |> float) > per
                        then
                            per <- (sugar |> float) / ((water + sugar) |> float)
                            ans <- (water + sugar,sugar)

printfn "%d %d" (fst ans) (snd ans)