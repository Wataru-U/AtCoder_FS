stdin.ReadLine() 
|> int
|> fun x -> Seq.init x (fun _ -> stdin.ReadLine())
|> Seq.countBy id
|> Seq.filter (snd >> fun x -> x % 2 = 1)
|> Seq.length
|> printfn "%d"