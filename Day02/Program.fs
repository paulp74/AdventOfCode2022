open System

module Day02 =

    let pWin = 6
    let pDraw = 3

    let lines = System.IO.File.ReadAllLines(@"input.txt")

    type Choice =
        | Rock of score : int
        | Paper of score : int
        | Scissors of score: int

    let rock = Rock(1)
    let paper = Paper(2)
    let scissors = Scissors(3)

    let mapLineToChoicesStrat1 (s:string) =
        let mapLetterToChoice s =
            match s with
            | "A" | "X" -> rock
            | "B" | "Y" -> paper
            | "C" | "Z" -> scissors

        match s.Split(' ') with
        | [|l; r|] -> mapLetterToChoice l, mapLetterToChoice r
        | _ -> raise (System.ArgumentException("Dodgy input line"))

    let mapLineToChoicesStrat2 (s:string) =
        let mapLetterToChoice s =
            match s with
            | "A" -> rock
            | "B" -> paper
            | "C" -> scissors

        let mapStrategyToChoice choice s =
            match s, choice with
            | "Y", c -> c
            | "X", Rock(_) -> scissors
            | "X", Paper(_) -> rock
            | "X", Scissors(_) -> paper
            | "Z", Rock(_) -> paper
            | "Z", Paper(_) -> scissors
            | "Z", Scissors(_) -> rock
            |  _ -> raise (System.ArgumentException("Dodgy input line"))

        match s.Split(' ') with
        | [|l; r|] ->
            let lChoice = mapLetterToChoice l
            let rChoice = mapStrategyToChoice lChoice r
            lChoice, rChoice
        | _ -> raise (System.ArgumentException("Dodgy input line"))

    let rpsRounds = lines |> Array.map mapLineToChoicesStrat2

    let calcScore (l, r) =
        match l, r with
        | Rock(sl), Rock(sr)            -> sl+pDraw, sr+pDraw
        | Paper(sl), Paper(sr)          -> sl+pDraw, sr+pDraw
        | Scissors(sl), Scissors(sr)    -> sl+pDraw, sr+pDraw
        | Rock(sl), Scissors(sr)        -> sl+pWin, sr
        | Rock(sl), Paper(sr)           -> sl, sr+pWin
        | Paper(sl), Rock(sr)           -> sl+pWin, sr
        | Paper(sl), Scissors(sr)       -> sl, sr+pWin
        | Scissors(sl), Rock(sr)        -> sl, sr+pWin
        | Scissors(sl), Paper(sr)       -> sl+pWin, sr

    let scores = rpsRounds |> Array.map calcScore

    let (lhs, rhs) = scores |> Array.fold (fun (lacc, racc) (l, r) -> lacc+l, racc+r) (0, 0)

    printfn "His Score: %i; My Score: %i" lhs rhs