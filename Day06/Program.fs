let input = System.IO.File.ReadAllLines(@"input.txt").[0]

let findMarker nDistinctChars =
    input
    |> Seq.windowed nDistinctChars
    |> Seq.findIndex (fun el -> (Set.ofSeq el).Count = nDistinctChars)
    |> (+) nDistinctChars

printfn "P1 answer: %i" (findMarker 4)
printfn "P2 answer: %i" (findMarker 14)