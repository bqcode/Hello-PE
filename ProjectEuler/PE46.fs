module PE46

let isOk n =
    match n with
    | n when n % 2 = 1 -> false
    | n -> 
        let step1 = n / 2
        let root = sqrt <| float step1 |> int
        (root * root) = step1

let checkOdd (primes : bool []) odd =
    let rec foo i found =
        match i, found with
        | _, true -> true
        | k, _ when k < 2 -> false
        | k, _ when not primes.[k] -> foo (k-2) found
        | _, _ ->
            let ok = isOk (odd-i)
            foo (i-2) ok
    foo (odd-2) false


let solve n =
    let primes = Common.getPrimesSieve n
    let oddNums = seq { for i in 9..2..n do if not primes.[i] then yield i }
    System.Threading.Tasks.Parallel.ForEach(oddNums,
        (fun odd -> 
            //for odd in oddNums do
            let isSum = checkOdd primes odd
            if not isSum then printfn "Not odd %d" odd
        )) |> ignore
    ()