module Common

/// Get unique elements from a sorted list
let getUniqueElements xs =
    let rec unique' xs ys =
        match xs with
        | [] -> ys
        | [x] -> x :: ys
        | x1 :: x2 :: xs -> if x1 = x2 then unique' (x2::xs) ys else unique' (x2::xs) (x1::ys)
    unique' xs []

/// Get sieve of primes up to and including n
let getPrimesSieve n =
    let sieve = Array.create (n+1) true
    sieve.[0] <- false
    sieve.[1] <- false
    for i in 2 .. n do
        if sieve.[i] then
            for j in (2*i) .. i .. n do
                sieve.[j] <- false
    sieve

/// Convert primes sieve to Array of prime numbers
let toPrimes sieve = 
    let n = Array.length sieve
    Array.zip sieve [|0..(n-1)|]
    |> Array.filter (fun (p, _) -> p)
    |> Array.Parallel.map (fun (_, i) -> i)

/// Factorize number n using a primes array
let factorize primes n =
    let rec factorize' x acc =
        match x with
        | 1 -> acc
        | _ -> 
            let divisor = primes |> Array.tryFind (fun p -> x % p = 0)
            match divisor with
            | None -> factorize' (x/x) (x :: acc)
            | Some(d) -> factorize' (x/d) (d :: acc)
    factorize' n []

/// Backtracking algorithm
let backtrac isSolution processSolution generateCandidates n =
    let rec backtrack' vec k =
        if isSolution vec k n then
            processSolution vec k n
        else
            let candidates = generateCandidates vec k n
            for candidate in candidates do
                backtrack' (candidate::vec) (k+1)
    backtrack' [] 0

/// Get n-element permutations
let getPermutations n =
    let isSolution _ k n = k = n
    let processSolution vec _ _ =
        vec
        |> List.rev
        |> printfn "%A"
    let generateCandidates vec k n =
        let used = Array.create n false
        for x in vec do
            used.[x-1] <- true
        Array.zip used [|1..n|]
        |> Array.filter (fun (b, _) -> not b)
        |> Array.map (fun (_, x) -> x)
        |> Array.toList
    backtrac isSolution processSolution generateCandidates n

/// Checks whether all elements in the list are the same
let allElementsAreSame xs = 
    List.fold (fun (x,b) y -> if x <> y then (y, true) else (y, false || b)) (List.head xs, false) xs
    |> snd |> not

/// Converts number to a list of digits
let numAsList x = 
    let rec numAsList' x acc = 
        match x with
        | 0 -> acc
        | _ -> numAsList' (x/10) ((x%10)::acc)
    numAsList' x []

/// Checks whether the two numbers contains the same digits
let sameDigits x y =
    let x' = numAsList x |> List.sort
    let y' = numAsList y |> List.sort
    List.length x' = List.length y' && List.forall2 (=) x' y'
