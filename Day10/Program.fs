open System.Text.RegularExpressions

module Day10 =
    // Read in input and generate commands
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    type Command =
        | Noop of cycles : int
        | Addx of cycles : int * v : int

    let lines = System.IO.File.ReadAllLines(@"input.txt") |> List.ofArray
    let mapper s =
        match s with
        | Regex @"addx (.*)" [ v ] -> Addx(2, int v)
        | Regex @"noop" _ -> Noop(1)
        | _ -> raise(System.ArgumentException(sprintf "Unexpected command %s" s))

    let commands = lines |> List.map mapper

    let mutable x = 1
    let genericCycleFolder f ((currentCommand, commandCycles), commandsLeft, argF) cycle =
        let outF = f cycle x argF // act upon value of X to update acc

        let commandCycles = commandCycles + 1
        let c, commandsLeft = 
            let getNext() =
                match commandsLeft with
                | h :: t -> (h, 0), t
                | _ -> (Noop(1), 1), List.empty

            match currentCommand with
            | Addx(c, v) when c = commandCycles ->
                x <- x + v
                getNext()
            | Noop(c)    when c = commandCycles ->
                ()
                getNext()
            | _ ->
                (currentCommand, commandCycles), commandsLeft
        c, commandsLeft, outF

    // Part 1
    x <- 1
    let signalStrength' cycle x signalStrength =
            if (cycle - 20)%40 = 0 then
                signalStrength + cycle*x
            else
                signalStrength
    let p1CycleFolder' = genericCycleFolder signalStrength'
    let getSolutionP1'() =
        match commands with
        | h::t ->
            let _, _, signalStrength = [|1..220|] |> Array.fold p1CycleFolder' ((h, 0), t, 0)
            signalStrength
        | _    ->
            raise(System.ArgumentException("Expected more commands"))
    printfn "Part1: %i" (getSolutionP1'())

    // Part 2
    x <- 1
    let pixels' cycle x pixels = (if abs(x-(cycle-1)%40) > 1 then " " else "#") :: pixels
    let p2CycleFolder' = genericCycleFolder pixels'

    match commands with
    | h::t ->
        let _, _, pixels = [|1..240|] |> Array.fold p2CycleFolder' ((h, 0), t, List.empty)
        pixels
        |> List.rev
        |> List.chunkBySize 40
        |> List.map (fun el -> el |> String.concat "")
        |> List.iter (printfn "%s")
    | _    ->
        raise(System.ArgumentException("Expected more commands"))

   