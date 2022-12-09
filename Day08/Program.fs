module Day08 =

    let forest =
        System.IO.File.ReadAllLines(@"input.txt")
        |> Array.map (fun s -> s.ToCharArray() |> Array.map (int))
        |> array2D

    let mutable n = 0

    let up row col      = forest.[0..row-1, col] |> Array.rev
    let down row col    = forest.[row+1.., col]
    let left row col    = forest.[row, 0..col-1] |> Array.rev
    let right row col   = forest.[row, col+1..] 

    let checkVisible row col height =
        if  row = 0
            || col = 0
            || row = forest.GetUpperBound(0)
            || col = forest.GetUpperBound(1)
        then
            n <- n+1
        else
            let isVisible (trees:seq<int>) =
                trees |> Seq.exists (fun el -> el >= height) |> not

            if     isVisible (up row col)
                || isVisible (down row col)
                || isVisible (left row col)
                || isVisible (right row col) then
                    n <- n+1

    forest |> Array2D.iteri checkVisible
    printfn "Part 1: %i" n

    let mutable maxScenicScore = 0

    let scenicScore row col height =
        if  row = 0
            || col = 0
            || row = forest.GetUpperBound(0)
            || col = forest.GetUpperBound(1)
        then
            ()
        else
            let numTrees (trees:seq<int>) =
                let oIndex = trees |> Seq.tryFindIndex (fun t -> t >= height)
                match oIndex with
                | Some i -> i + 1
                | None   -> trees |> Seq.length

            let scenicScore =
                numTrees (up row col)
                * numTrees (down row col)
                * numTrees (left row col)  
                * numTrees (right row col)

            maxScenicScore <- max scenicScore maxScenicScore

    forest |> Array2D.iteri scenicScore
    printfn "Part 2: %i" maxScenicScore  

