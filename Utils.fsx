module Solution =
    open System.Diagnostics

    let run name f input =
        let sw = Stopwatch.StartNew ()
        let result = f input
        let elapsedMs = sw.ElapsedMilliseconds
        printfn "%s completed in %dms with result: %A" name elapsedMs result