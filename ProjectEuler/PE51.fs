module PE51

let solve n =
    let sieve = Common.getPrimesSieve n
    let primes = sieve |> Common.toPrimes
    primes
