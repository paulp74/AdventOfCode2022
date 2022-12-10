module Day09 =
    let moves =
        System.IO.File.ReadAllLines(@"input.txt")
        |> Seq.map (fun s ->
                        let parts = s.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
                        parts.[0], int parts.[1])

    let getNewTailPosition (xH, yH) (xT, yT) =
        match xH-xT, yH-yT with
        | dx, dy when abs dx > 1 || abs dy > 1 -> (xT + 1 * sign(dx), yT + 1 * sign(dy))
        | _ -> (xT, yT)

    let moveFolder (head, tail, tailXYs) (dir, steps) =
        let dx, dy =
            match dir with
            | "R" -> 1, 0
            | "L" -> -1, 0
            | "U" -> 0, 1
            | "D" -> 0, -1
        let stepFolder ((xH, yH), tail, tailXYs) _ =
            let headNew = xH + dx, yH + dy
            let tailNew = getNewTailPosition headNew tail
            headNew, tailNew, tailXYs |> Set.add tailNew
        [1..steps] |> Seq.fold stepFolder (head, tail, tailXYs)

    let head, tail, tailXYs = moves |> Seq.fold moveFolder ((0,0), (0,0), Set.empty |> Set.add (0,0))

    printfn "Part 1, answer: %i" tailXYs.Count