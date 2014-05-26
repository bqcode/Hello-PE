module Backtrack

let pandigitals =
    let suma = ref 0uL
    let toNumber v =
        let rec toNumber' (xs : int list) (m : uint64) (acc : uint64) =
            match xs with
            | [] -> acc
            | x :: xs -> toNumber' xs (m*10uL) (acc+m*(uint64 x))
        toNumber' v 1uL 0uL
    let isSolution vec k n = 
        if k <> n then false
        else
            let vec' = vec |> List.rev |> List.toArray
            (vec'.[1] * 100 + vec'.[2] * 10 + vec'.[3]) % 2 = 0 &&
            (vec'.[2] * 100 + vec'.[3] * 10 + vec'.[4]) % 3 = 0 &&
            (vec'.[3] * 100 + vec'.[4] * 10 + vec'.[5]) % 5 = 0 &&
            (vec'.[4] * 100 + vec'.[5] * 10 + vec'.[6]) % 7 = 0 &&
            (vec'.[5] * 100 + vec'.[6] * 10 + vec'.[7]) % 11 = 0 &&
            (vec'.[6] * 100 + vec'.[7] * 10 + vec'.[8]) % 13 = 0 &&
            (vec'.[7] * 100 + vec'.[8] * 10 + vec'.[9]) % 17 = 0
    let processSolution vec _ _ =
        vec
        |> List.rev
        |> printfn "%A"
        suma := !suma + (toNumber vec)
    let generateCandidates vec k n =
        let used = Array.create n false
        if k = 0 then used.[0] <- true
        for x in vec do
            used.[x] <- true
        Array.zip used [|0..(n-1)|]
        |> Array.filter (fun (b, _) -> not b)
        |> Array.map (fun (_, x) -> x)
        |> Array.toList
    Common.backtrac isSolution processSolution generateCandidates 10
    !suma