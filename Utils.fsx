let tee f x = f x; x

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


module Regex =
    open System.Text.RegularExpressions

    let ``match`` pattern input =
        Regex.Match(input, pattern).Groups

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
