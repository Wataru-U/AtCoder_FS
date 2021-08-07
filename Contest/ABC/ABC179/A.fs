let s = stdin.ReadLine();
let ans = if s.[s.Length-1] = 's' then s + "es" else s + "s" 
ans |> printfn "%s"