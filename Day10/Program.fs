open System.Text.RegularExpressions

module Day10 =
    // Read in input and generate commands
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    type Command =
        | Noop
        | Addx of v : int

    let mapper s =
        match s with
        | Regex @"addx (.*)" [ v ] -> [ Noop; Addx(int v)] // Adding in extra null ops really simplifies the folder logic
        | Regex @"noop" _ -> [ Noop ]
        | _ -> raise(System.ArgumentException(sprintf "Unexpected command %s" s))

    let denormalisedCommands =
        System.IO.File.ReadAllLines(@"input.txt")
        |> List.ofArray
        |> List.collect mapper

    let mutable x = 1
    let genericCycleFolder f (currentCommand, commandsLeft, argF) cycle =
        let outF = f cycle x argF // act upon value of X to update acc

        match currentCommand with
        | Addx(v)   -> x <- x + v
        | Noop      -> ()
               
        match commandsLeft with
        | h :: t    -> h, t, outF
        | _         -> Noop, List.empty, outF

    // Part 1
    x <- 1
    let signalStrength' cycle x signalStrength =
            signalStrength + if (cycle - 20)%40 = 0 then cycle*x else 0

    let p1CycleFolder = genericCycleFolder signalStrength'
    let getSolutionP1'() =
        match denormalisedCommands with
        | h::t ->
            let _, _, signalStrength = [|1..220|] |> Array.fold p1CycleFolder (h, t, 0)
            signalStrength
        | _    ->
            raise(System.ArgumentException("Expected more commands"))
    printfn "Part1: %i" (getSolutionP1'())

    // Part 2
    x <- 1
    let pixels' cycle x pixels = (if abs(x-(cycle-1)%40) > 1 then " " else "#") :: pixels
    let p2CycleFolder = genericCycleFolder pixels'

    match denormalisedCommands with
    | h::t ->
        let _, _, pixels = [|1..240|] |> Array.fold p2CycleFolder (h, t, List.empty)
        pixels
        |> List.rev
        |> List.chunkBySize 40
        |> List.map (fun el -> el |> String.concat "")
        |> List.iter (printfn "%s")
    | _    ->
        raise(System.ArgumentException("Expected more commands"))

   