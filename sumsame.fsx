// sumByDigits [ 2 .. 3 ]  = 5
// sumByDigits [ 11 .. 13 ] = 9
let sumByDigits: (int seq -> int) = 
    Seq.collect string 
    >> Seq.map (string >> int) 
    >> Seq.sum

// infiniteSeq 1 1
// infiniteSeq 5 -1 |> Seq.iter (printfn "%A")
let rec infiniteSeq incr from = 
    seq {
        if from > 0 then
            yield from
            yield! infiniteSeq incr (incr + from) }

// subsets [ 1 .. 5] |> Seq.length |> (=) 5
let subsets xs =        
    let rec subsets' n preLen xs =
        seq {
            let fstN = Seq.truncate n xs
            let len = Seq.length fstN
            if len <> preLen then
                yield fstN
                yield! subsets' (n + 1) len xs }

    subsets' 1 0 xs

// findSeqsBySum 52 [ 7 .. 16 ]
let findSeqsBySum sum = 
    subsets
    >> Seq.takeWhile (fun xs' -> sumByDigits xs' <= sum)
    >> Seq.filter (fun xs' -> sumByDigits xs' = sum)

// findBetween 5 10
// findBetween 7 16
// findBetween 99 110 // 4 
// findBetween 1000 1050 // 13 results, quite fast
// findBetween 1000 1100 // 21 results, a bit laggy
// findBetween 1000 1200 // 36, still works but 5 seconds wait
// findBetween 1000 1300 // 10 seconds!
// findBetween 1000 2000 // finishes in 35 seconds, but iterating will freeze
let findBetween (a: int) (b: int) =
    seq {
        let sum = sumByDigits [ a .. b ]
        for (xs, incr) in [ ([ a .. b ], 1); ([ b - 1 .. -1 .. a ], -1) ] do
            yield! 
                xs 
                |> Seq.map (infiniteSeq incr) 
                |> Seq.collect (findSeqsBySum sum) }   
    |> Seq.map (fun xs -> (Seq.head xs, Seq.last xs ))