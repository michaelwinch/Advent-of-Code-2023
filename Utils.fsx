let tee f x = f x; x

let log x = printfn "LOG: %A" x
let logm m x = printfn "%s: %A" m x

module Solution =
    open System.Diagnostics

    let run name f input =
        let sw = Stopwatch.StartNew ()
        let result = f input
        let elapsedMs = sw.ElapsedMilliseconds
        printfn "%s completed in %dms with result: %A" name elapsedMs result


module File =
    open System.IO
    
    let readStream (inputFile: string) =
        seq {
            use streamReader = new StreamReader(inputFile)
            while not streamReader.EndOfStream do
                yield streamReader.ReadLine()
        }


module String =
    open System

    let (|IsNullOrWhitespace|_|) (x: string) =
        if String.IsNullOrWhiteSpace x then Some () else None


module Maths =
    let highestCommonFactor x y =
        let lower, higher = if x < y then x, y else y, x
        let rec loop lower higher =
            if lower = 0L then higher
            else
                loop (higher % lower) lower
        loop lower higher

    let lowestCommonMultiple x y =
        x * y / highestCommonFactor x y


module Regex =
    open System.Text.RegularExpressions

    let ``match`` pattern input =
        Regex.Match(input, pattern).Groups

    let matches pattern input =
        Regex.Matches(input, pattern)

    let (|Match|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some m.Groups
        else None

    let (|Matches|_|) pattern input =
        let m = Regex.Matches(input, pattern)
        if m.Count > 0 then Some m else None

    let (|MatchesAlways|_|) pattern input =
        let m = Regex.Matches(input, pattern)
        Some m
