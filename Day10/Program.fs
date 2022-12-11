open System.Text.RegularExpressions

module Day10 =
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
    let p1folder ((currentCommand, cyclesLeft), commandsLeft, signalStrength) cycle =
        let signalStrength = 
            if (cycle - 20)%40 = 0 then
                signalStrength + cycle*x
            else
                signalStrength

        let cyclesLeft = cyclesLeft - 1
        let c, commandsLeft = 
            if cyclesLeft = 0 then
                // execute command
                match currentCommand with
                | Addx(_, v)    -> x <- x + v
                | Noop(_)       -> ()

                match commandsLeft with
                | h :: t -> match h with
                            | Addx(c, _) -> (h, c), t
                            | Noop(c)    -> (h, c), t
                | _ -> (Noop(1), 1), List.empty
            else
                (currentCommand, cyclesLeft), commandsLeft
        
        c, commandsLeft, signalStrength

    let getSolutionP1() =
        let h = commands |> List.head
        let t = commands |> List.tail

        let c = match h with
                | Addx(c, _) -> (h, c)
                | Noop(c)    -> (h, c)

        let _, _, signalStrength = [|1..220|] |> Array.fold p1folder (c, t, 0)
        signalStrength

    printfn "Part1: %i" (getSolutionP1())

    x <- 1
    let p2folder ((currentCommand, cyclesLeft), commandsLeft, pixels) cycle =
        let pixel = if abs(x-(cycle-1)%40) > 1 then " " else "#"

        let cyclesLeft = cyclesLeft - 1
        let c, commandsLeft = 
            if cyclesLeft = 0 then
                // execute command
                match currentCommand with
                | Addx(_, v)    -> x <- x + v
                | Noop(_)       -> ()

                match commandsLeft with
                | h :: t -> match h with
                            | Addx(c, _) -> (h, c), t
                            | Noop(c)    -> (h, c), t
                | _ -> (Noop(1), 1), List.empty
            else
                (currentCommand, cyclesLeft), commandsLeft
        
        c, commandsLeft, pixel::pixels

    let getSolutionP2() =
        let h = commands |> List.head
        let t = commands |> List.tail

        let c = match h with
                | Addx(c, _) -> (h, c)
                | Noop(c)    -> (h, c)

        let _, _, pixels = [|1..240|] |> Array.fold p2folder (c, t, List.empty)
        pixels
        |> List.rev
        |> List.chunkBySize 40
        |> List.map (fun el -> el |> String.concat "")
        |> List.iter (printfn "%s")

    getSolutionP2() 