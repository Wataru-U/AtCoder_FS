let [|a;b;c|] = stdin.ReadLine().Split()
if a = b then c
elif a = c then b
elif b = c then a
else "0"
|> printfn "%s" 
