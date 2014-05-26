module PE47

let solve n =
    let what = 4
    let rec find4 xs y elem cnt =
        match xs, y, elem, cnt with
        | _,_,_,4 -> Some(elem)
        | [],_,_,_ -> None
        | (e, x)::xs,_,_,_ ->
            if x = y && x = what then find4 xs x elem (cnt+1)
            else find4 xs x e 1
    let primes = Common.getPrimes (int (sqrt <| float n)) |> Common.toPrimes
    let uniqueFactors = Array.Parallel.map (fun x -> (x, Common.factorize primes x |> Common.unique |> List.length)) [|1..n|] |> Array.toList
    find4 uniqueFactors -1 -1 0
