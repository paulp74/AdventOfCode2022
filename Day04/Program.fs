let lines = System.IO.File.ReadAllLines(@"input.txt")

let mapper (s:string) =
    let rangeToPair (s:string) =
        match s.Split('-') with
        | [|l; r|] -> int l, int r
        | _ -> raise(System.ArgumentException(sprintf "Dodgy input range: %s" s))

    match s.Split(',') with
    | [|l; r|] -> rangeToPair l, rangeToPair r
    | _ -> raise(System.ArgumentException(sprintf "Dodgy input: %s" s))

let isContainedBy ((lhsStart, lhsEnd), (rhsStart, rhsEnd)) =
    (lhsStart >= rhsStart && lhsEnd <= rhsEnd)
    || (rhsStart >= lhsStart && rhsEnd <= lhsEnd)

let p1Answer =
    lines
    |> Array.map mapper
    |> Array.filter isContainedBy
    |> Array.length

printfn "Part1: %i" p1Answer

let isOverlapping ((lhsStart, lhsEnd), (rhsStart, rhsEnd)) =
    (lhsStart <= rhsStart && lhsEnd >= rhsStart) ||
    (lhsStart > rhsStart && lhsStart <= rhsEnd)

let p2Answer =
    lines
    |> Array.map mapper
    |> Array.filter isOverlapping
    |> Array.length

printfn "Part1: %i" p2Answer