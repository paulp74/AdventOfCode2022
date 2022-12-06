module Day03 =
    //let lines = System.IO.File.ReadAllLines(@"input.txt")
    //let allChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".ToCharArray()

    //let findPriority c = (if c >= 'a' then (int c - int 'a') else (int c - int 'A') + 26) + 1

    //let mapper (line:string) =
    //    let chars = line.ToCharArray()
    //    let charsL = chars.[..(chars.Length/2-1)] |> Set.ofArray
    //    let charsR = chars.[(chars.Length/2)..]   |> Set.ofArray
    //    let dodgyChar =
    //        let intersect = Set.intersect charsL charsR
    //        if intersect.Count = 1 then intersect.MaximumElement else raise (System.ArgumentException("Dodgy input"))
    //    findPriority dodgyChar

    //let answer = lines |> Array.map mapper |> Array.sum

    //printfn "Answer: %i" answer

    let lines = System.IO.File.ReadAllLines(@"input.txt")
    let findPriority c = (if c >= 'a' then (int c - int 'a') else (int c - int 'A') + 26) + 1
    let p2answer =
        lines
        |> Array.map (fun s -> s.ToCharArray() |> Set.ofArray)
        |> Array.chunkBySize 3
        |> Array.map (fun group -> group |> Array.reduce (Set.intersect) |> Set.maxElement |> findPriority)
        |> Array.sum

    let p2answer' =
        lines
        |> Seq.map (fun s -> s.ToCharArray() |> Set.ofArray)
        |> Seq.chunkBySize 3
        |> Seq.collect Set.intersectMany
        |> Seq.map findPriority
        |> Seq.sum


    printfn "Part 2: %i" p2answer
    printfn "Part 2: %i" p2answer'


    