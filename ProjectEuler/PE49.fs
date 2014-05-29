module PE49
open System.Collections.Concurrent

let solve n = 
    let bag = ConcurrentBag<int list>()
    let solveForGroup (grp : int []) = 
        let rec genDifs acc xs =
            match xs with
            | [] | [_] -> acc
            | x::y::ys -> genDifs ((y-x)::acc) (y::ys)
        let toNumbers xs = xs |> List.mapi (fun idx x -> if x then [grp.[idx]] else []) |> List.concat
        let isSolution vec k n =
            let numbersUsed = List.fold (fun a x -> match x with | true -> a+1 | _ -> a) 0 vec
            if k <> n || numbersUsed < 3 then false
            else
                vec |> toNumbers |> genDifs [] |> Common.allElementsAreSame
        let processSolution vec _ _ =
            vec |> toNumbers |> bag.Add
        let generateCandidates _ k n = if k > n then [] else [true;false]
        Common.backtrac isSolution processSolution generateCandidates (Array.length grp)
    let areDifferent n = 
        let asList = Common.numAsList n
        let _, r = asList |> List.sort |> List.fold (fun (x,e) y -> if x = y then (x, true) else (y, false || e)) (-1, false)
        not r
    let primes = 
        Common.getPrimesSieve n
        |> Common.toPrimes 
        |> Array.Parallel.choose (fun x -> if x > 1000 then Some(x) else None)
    let used = Array.create (Array.length primes) false
    primes |> Array.iteri (fun idx p -> 
        if not used.[idx] 
        then 
            primes |> Array.Parallel.iteri (fun usedIdx p' -> if p <> p' && Common.sameDigits p p' then used.[usedIdx] <- true))
    let primesGroups =
        primes
        |> Array.Parallel.mapi 
            (fun idx x -> 
                if not used.[idx] 
                then Array.Parallel.choose (fun y -> if Common.sameDigits x y then Some(y) else None) primes
                else Array.empty)
    primesGroups
        |> Array.filter (fun xs -> Array.length xs > 2)
        |> Array.Parallel.map Array.sort
        |> Array.Parallel.iter solveForGroup
    bag