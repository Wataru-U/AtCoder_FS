open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text

new StreamWriter(Console.OpenStandardOutput())
|> fun x -> x.AutoFlush <- false; x
|> Console.SetOut

// ここに処理

// ここまで

stdout.Flush()
