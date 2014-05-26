module PE49

let solve = 
    let rec numAsList x acc = 
        match x with
        | 0 -> acc
        | _ -> numAsList (x/10) ((x%10)::acc)
    let areDifferent n = 
        let asList = numAsList n []
        let _, r = asList |> List.sort |> List.fold (fun (x,e) y -> if x = y then (x, true) else (y, false || e)) (-1, false)
        not r
    let primes = 
        Common.getPrimes 10000 
        |> Common.toPrimes 
        |> Array.Parallel.choose (fun x -> if x > 1000 && areDifferent x then Some(x) else None)
    primes
