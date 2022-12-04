// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

module Day01 =

    let maxCals lines =
        let folder (elfSum, maxCals) s =
            match s with
            | "" -> (0, maxCals)
            | s  -> 
                let runningTotal = elfSum + (int s)
                (runningTotal, max runningTotal maxCals)
        
        lines |> Array.fold folder (0, 0) |> snd

    // read in the values as array of string
    let lines = System.IO.File.ReadAllLines(@"input.txt")
    printfn "MaxCals = %i" (maxCals lines)

    let topN n lines =
        let folder2 (elfSum, elfSums) s =
            match s with
            | ""    -> (0, elfSum :: elfSums)
            | s     -> (elfSum + (int s), elfSums)

        let (finalSum, elfSums) = lines |> Array.fold folder2 (0, List.empty)
        let elfSums = finalSum :: elfSums |> List.sort |> List.rev
        elfSums |> List.take n |> List.sum

    printfn "Top 3 sum = %i" (topN 3 lines)

