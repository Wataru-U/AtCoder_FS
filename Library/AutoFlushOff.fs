open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text
let sw =
    new StreamWriter(Console.OpenStandardOutput())
    |> fun x -> x.AutoFlush <- false; x
Console.SetOut(sw)

// ここに処理


// ここまで

sw.Flush()