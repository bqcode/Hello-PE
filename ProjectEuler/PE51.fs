module PE51

let generateDiffs pos =
    let createDiff xs = 
        List.zip pos xs
        |> List.map (fun (p,used) -> if used then pown 10 p else 0)
        |> List.sum
    Common.getSubsets <|List.length pos 
    |> Array.filter (fun xs -> xs |> List.exists id)
    |> Array.map createDiff
    |> Array.toList

let getNumbersToAdd maxDigit p =
    let asList = Common.numAsList p |> List.rev
    let rec getNumToAdd' maxD acc =
        if maxD >= maxDigit then acc
        else
            let pos = 
                asList 
                |> List.mapi (fun idx x -> if x = maxD then (idx, true) else (idx, false))
                |> List.choose (fun (idx, f) -> if f then Some(idx) else None)
            if List.length pos = 0 then getNumToAdd' (maxD+1) acc
            else
                let diffs = 
                    generateDiffs pos
                    |> List.map (fun diff -> [1..(10-maxD-1)] |> List.map ((*) diff))
                getNumToAdd' (maxD+1) (diffs::acc)
    let baseDiffs = getNumToAdd' 0 [] |> List.concat
    baseDiffs
        
let solve maxDigit n =
    let isCandidate maxDigit p = Common.numAsList p |> List.exists ((>) maxDigit)
    let sieve = Common.getPrimesSieve n
    let primes = sieve |> Common.toPrimes
    let isPrime x = x <= n && sieve.[x]
    let doAdding x xs = xs |> List.map ((+) x) |> List.filter isPrime |> List.length |> (+) 1
    let primesWithDiffs = 
        primes 
        |> Array.filter ((<) 10)
        |> Array.filter (isCandidate maxDigit)
        |> Array.Parallel.map (fun x -> (x, getNumbersToAdd maxDigit x |> List.map (doAdding x) |> List.max))
        |> Array.filter (fun (_, cnt) -> cnt > (10-maxDigit-1))

    primesWithDiffs
