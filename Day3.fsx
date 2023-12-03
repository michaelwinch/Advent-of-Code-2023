//#time
#load "Utils.fsx"

open System
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
        matches
        |> Seq.filter (fun m -> hasAdjacentSymbol lines (currentLine.IndexOf m.Value) m.Value.Length)
        |> Seq.map (fun m -> int m.Value)
        |> List.ofSeq
    | _ -> []


let run inputFile =
    File.readStream inputFile
    |> wrapEmptyLines inputFile
    |> Seq.windowed 3
    |> Seq.collect getPartNumbers
    |> Seq.sum

run "Inputs/Actual/Day3.txt"
//|> Seq.iteri (fun idx x -> if idx < 100 then printfn "%A" x)