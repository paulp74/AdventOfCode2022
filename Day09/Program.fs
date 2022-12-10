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

    let moveHead dir (x, y) =
        let dx, dy =
            match dir with
            | "R" -> 1, 0
            | "L" -> -1, 0
            | "U" -> 0, 1
            | "D" -> 0, -1
        x + dx, y + dy

    let moveFolder (head, tail, tailHistory) (dir, steps) =
        let tailFolder (head, tail) t =
            let tNew = getNewTailPosition head t
            (tNew, tNew::tail)

        let stepFolder (head, tail, tailHistory) _ =
            let headNew = moveHead dir head
            let (tLast, tail) = tail |> List.fold tailFolder (headNew, List.empty)
            headNew, tail |> List.rev, tailHistory |> Set.add tLast

        [1..steps] |> Seq.fold stepFolder (head, tail, tailHistory)

    let findSolution tailLength =
        moves |> Seq.fold moveFolder ((0,0), List.replicate tailLength (0,0), Set.empty |> Set.add (0,0))

    let _, _, tailHistoryP1 = findSolution 1
    printfn "Part 1, answer: %i" tailHistoryP1.Count

    let _, _, tailHistoryP2 = findSolution 9
    printfn "Part 2, answer: %i" tailHistoryP2.Count