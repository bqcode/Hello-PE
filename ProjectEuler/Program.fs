// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let sol = PE49.solve
    sw.Stop()
    printfn "Primes = %A" sol
    printfn "Len = %d" <| Array.length sol
    printfn "Time taken = %s" <| sw.Elapsed.ToString()
    stdin.ReadLine() |> ignore
    0 // return an integer exit code
