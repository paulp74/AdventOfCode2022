open System.Text.RegularExpressions
open System.Collections.Generic

module Day07 =



    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None
    
    //Example:
    //let phone = "(555) 555-5555"
    //match phone with
    //| Regex @"\(([0-9]{3})\)[-. ]?([0-9]{3})[-. ]?([0-9]{4})" [ area; prefix; suffix ] ->
    //    printfn "Area: %s, Prefix: %s, Suffix: %s" area prefix suffix
    //| _ -> printfn "Not a phone number"

    type FileSystem =
        | Directory of name : string * contents : FileSystem list
        | File of name : string * size : int

    let lines = System.IO.File.ReadAllLines(@"testInput.txt")
    let stack = Stack(lines)

    let fromFile (fileLine : string) =
        let parts = fileLine.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        File(parts.[1], int parts.[0])

    let rec fromDir (name:string) =
        let i = lines |> Array.findIndex (fun el -> el = "$ cd " + name)
        // make the assumption that we can skip next line as it will be ls
        let dirs, files =
            lines.[i+2..]
            |> Array.takeWhile (fun el -> not (el.StartsWith('$')))
            |> Array.partition (fun el -> el.StartsWith("dir"))

        let dirs = dirs |> Array.map (fun s -> s.Substring(4))

        let contents = [
            yield! files |> Array.map fromFile
            yield! dirs  |> Array.map fromDir
        ]
        Directory(name, contents)

    let root = fromDir("/")


    let rec processDirectory (name : string) =
        let mutable line = ""
        let contents = [
            while (stack.Count > 0 && line <> "$ cd ..") do
                line <- stack.Pop()
                match line with
                | Regex @"\$ ls" _
                | Regex @"\$ cd .." _ -> ()
                | Regex @"\$ cd (.*)" [ dirName ] ->
                    yield processDirectory dirName
                | Regex @"(\d*) (.*)" [ size; name] ->
                    yield File(name, int size)
        ]
        Directory(name, contents)

    let processStack() =
        stack.Pop() |> ignore
        processDirectory("/")

    let root' = processStack()

    // traverse over the directory tree, working out sizes of each directory
    let rec sumContainedFiles (root : FileSystem) =
        match root with
        | Directory(_, contents) ->
            contents |> List.sumBy (fun c -> match c with
                                             | File(_, size) -> size
                                             | Directory(_) as d -> d |> sumContainedFiles)
        | _ -> 0

    let rec sumBigDirectories (root : FileSystem) =
        match root with
        | Directory(_, contents) as d ->
            let s = sumContainedFiles d
            (if s <= 100000 then s else 0) + (contents |> List.sumBy sumBigDirectories) 
        | _ -> 0

    printfn "Part 1 answer: %i" (sumBigDirectories root')

            


