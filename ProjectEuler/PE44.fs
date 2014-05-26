module PE44

let solve =
    let pentagon i = 
        (i * (3uL * i - 1uL)) / 2uL
    let isPentagon (a : uint64) = 
        let tmp = System.Math.Sqrt(float (24uL*a+1uL))
        let n = ((uint64 tmp) + 1uL) / 6uL
        let pentN = pentagon n
        a = pentN
    let rec bigger (n : uint64) =
        let biggerPent = pentagon n
        let smallerSeq = seq {for i in 1uL .. (n-1uL) -> pentagon i}
        let foo = 
            smallerSeq
            |> Seq.map (fun s -> (biggerPent+s, biggerPent-s))
            |> Seq.filter (fun (a, b) -> (isPentagon a) && (isPentagon b))
            |> Seq.toList
        if List.length foo > 0 then
            List.head foo
        else
            bigger (n+1uL)
    bigger 2uL
