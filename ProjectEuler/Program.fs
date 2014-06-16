
[<EntryPoint>]
let main argv = 
    let lines = 
        stdin.ReadToEnd().Split([| '\n' |])
        |> Array.toList
        |> List.filter (fun s -> s.Length = 29)
        |> List.map (fun s -> (new PE54.Hand(s.Substring(0, 14)), new PE54.Hand(s.Substring(15, 14))))
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let sol =
        lines
        |> List.map (fun (h1, h2) -> if h1.GetScore() < h2.GetScore() then 1 else 0)
        |> List.sum
    sw.Stop()
    printfn "Player one wins %d times." sol
    printfn "Time taken = %s" <| sw.Elapsed.ToString()
    stdin.ReadLine() |> ignore
    0 // return an integer exit code
