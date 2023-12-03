//#time
#load "Utils.fsx"

open System
open System.Text.RegularExpressions
open Utils

let wrapEmptyLines inputFile lines =
    let lineLength =
        System.IO.File.ReadLines inputFile
        |> Seq.head
        |> String.length

    let emptyLine =
        [| for _ in  1 .. lineLength -> '.' |]
        |> String

    seq {
        yield emptyLine
        yield! lines
        yield emptyLine
    }

let isSymbol (x: char) =
    Regex.``match`` "[^\.\d]" (string x)
    |> fun x -> x[0].Success

module Part1 =
    let hasAdjacentSymbol (lines: string array) startIndex length =
        lines
        |> Array.map (fun x -> x.ToCharArray())
        |> Array.exists (fun line ->
            line
            |> Array.mapi (fun idx char ->
                startIndex - 1 <= idx
                && idx <= startIndex + length
                && isSymbol char)
            |> Array.exists id)

    let getPartNumbers : string array -> int list =
        function
        | [| _; Regex.Matches "(\d+)" matches as currentLine; _ |] as lines ->
            let rec loop acc (startPos: int) =
                function
                | [] -> acc
                | m: Match :: t ->
                    let matchIndex = currentLine.IndexOf(m.Value, startPos)
                    let nextStartIndex = matchIndex + m.Value.Length

                    if hasAdjacentSymbol lines matchIndex m.Value.Length then
                        loop (int m.Value :: acc) nextStartIndex t
                    else loop acc nextStartIndex t

            loop [] 0 (matches |> List.ofSeq)
        | _ -> []

    let run inputFile =
        File.readStream inputFile
        |> wrapEmptyLines inputFile
        |> Seq.windowed 3
        |> Seq.collect getPartNumbers
        |> Seq.sum


module Part2 =
    let getGearIndexes (line: string) =
        line.ToCharArray()
        |> Array.mapi (fun idx char -> idx, char = '*')
        |> Array.filter snd
        |> Array.map fst
        |> List.ofArray

    let isPartAdjacentToGear gearIndex partIndex partLength =
        partIndex - 1 <= gearIndex
        && gearIndex <= partIndex + partLength

    let getAdjacentPartNumbersOnLine gearIndex (line: string) matches =
        let rec loop acc (startPos: int) =
            function
            | [] -> acc
            | m: Match :: t ->
                let matchIndex = line.IndexOf(m.Value, startPos)
                let nextStartIndex = matchIndex + m.Value.Length

                if isPartAdjacentToGear gearIndex matchIndex m.Value.Length then
                    loop (int m.Value :: acc) nextStartIndex t
                else loop acc nextStartIndex t

        loop [] 0 (matches |> List.ofSeq)

    let getGearRatios : string array -> int list =
        function
        | [| Regex.MatchesAlways "(\d+)" matches1 as lineBefore
             Regex.MatchesAlways "(\d+)" matches2 as currentLine
             Regex.MatchesAlways "(\d+)" matches3 as lineAfter |]
             ->
                getGearIndexes currentLine
                |> List.map (fun gearIndex ->
                    getAdjacentPartNumbersOnLine gearIndex lineBefore matches1
                    @ getAdjacentPartNumbersOnLine gearIndex currentLine matches2
                    @ getAdjacentPartNumbersOnLine gearIndex lineAfter matches3)
                |> List.filter (List.length >> (=) 2)
                |> List.map (List.reduce (*))
        | _ -> []

    let run inputFile =
        File.readStream inputFile
        |> wrapEmptyLines inputFile
        |> Seq.windowed 3
        |> Seq.collect getGearRatios
        |> Seq.sum

Solution.run "pt1" Part1.run "Inputs/Actual/Day3.txt" // pt1 completed in 7ms with result: 527364
Solution.run "pt2" Part2.run "Inputs/Actual/Day3.txt" // pt2 completed in 4ms with result: 79026871