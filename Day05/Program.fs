let lines = System.IO.File.ReadAllLines(@"input.txt")

// find the baseline (starting with 1) then partition input lines
let i = lines |> Array.findIndex (fun l -> l.Trim().StartsWith('1'))
let stackLines = lines.[..i-1]

// Pull the moves and starting stacks into the data model
// Stacks represented as array of lists (one list per stack)
// Moves are simply a 3-part tuple.
let moves = 
    let mapper (s:string) =
        let parts = s.Split(' ')
        (int parts.[1], int parts.[3] - 1, int parts.[5] - 1) // 0-based stacks

    lines.[i+2..] |> Array.map mapper

let numStacks = lines.[i].Split(' ', System.StringSplitOptions.RemoveEmptyEntries).Length

let getStacks()  =
    let stacks = Array.create numStacks (List.empty) // mutable, nasty

    let processStackHorizontal (s:string) =
        let horSlice = s.ToCharArray()
        for i in [0..numStacks-1] do
            let stackIndex = 1 + i * 4
            match horSlice.[stackIndex] with
            | ' ' -> ()
            | c   -> stacks.[i] <- c :: stacks.[i]

    lines.[..i-1]
    |> Array.rev
    |> Array.iter processStackHorizontal

    stacks

// Utility function to print out top of stacks
let printStackTops s =
    s |> Array.map (fun l -> l |> List.head) |> Array.iter (fun c -> printf "%c" c)
    printfn ""

// Part 1
let p1Stacks = getStacks()
let mover9000 (numItems, iFromStack, iToStack) =
    let rec move numItems fromStack toStack =
        match numItems with
        | 0 -> fromStack, toStack
        | n -> match fromStack with
               | h :: t -> move (n-1) t (h :: toStack)
               | []     -> raise(System.ArgumentException("Stack is empty"))
    let fromStack, toStack = move numItems p1Stacks.[iFromStack] p1Stacks.[iToStack]
    p1Stacks.[iFromStack] <- fromStack
    p1Stacks.[iToStack] <- toStack

moves |> Array.iter mover9000
p1Stacks |> printStackTops 

// Part 2
let p2Stacks = getStacks()
let mover9001 (numItems, iFromStack, iToStack) =
    let toMove, newFromStack = p2Stacks.[iFromStack] |> List.splitAt numItems
    p2Stacks.[iFromStack] <- newFromStack
    p2Stacks.[iToStack]   <- List.append toMove p2Stacks.[iToStack]

moves |> Array.iter mover9001
p2Stacks |> printStackTops