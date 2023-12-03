#time
#load "./Utils.fsx"

open System
open System.IO
open Utils

let getInput (inputFile: string) =
    seq {
        use streamReader = new StreamReader(inputFile)
        while not streamReader.EndOfStream do
            yield streamReader.ReadLine()
    }

module Part1 =
    let isDigit (x: char) =
        ['0'..'9']
        |> List.contains x

    let findFirstAndLastDigits (str: string) =
        let chars = str.ToCharArray()
        chars |> Array.find isDigit,
        chars |> Array.findBack isDigit

    let convertToNumber (d1: char, d2: char) =
        [|d1; d2|]
        |> String
        |> int

    let run inputFile =
        getInput inputFile
        |> Seq.map (findFirstAndLastDigits >> convertToNumber)
        |> Seq.sum


module Part2 =
    let digitMap =
        ([ '0'..'9' ]
        |> List.map (fun x -> string x, x))
        @
        [ "one", '1'
          "two", '2'
          "three", '3'
          "four", '4'
          "five", '5'
          "six", '6'
          "seven", '7'
          "eight", '8'
          "nine", '9' ]
        |> Map

    let (|StartsWithNumber|_|) (str: string) =
        Map.keys digitMap
        |> Seq.tryFind str.StartsWith
        |> Option.map (fun key -> Map.find key digitMap)

    let (|EndsWithNumber|_|) (str: string) =
        Map.keys digitMap
        |> Seq.tryFind str.EndsWith
        |> Option.map (fun key -> Map.find key digitMap)

    let findFirstDigit (str: string) =
        let rec loop =
            function
            | StartsWithNumber x -> x
            | x -> loop (x.Substring 1)
        loop str

    let findLastDigit (str: string) =
        let rec loop =
            function
            | EndsWithNumber x -> x
            | x -> loop (x.Substring(0, x.Length - 1))
        loop str

    let findFirstAndLastDigits (str: string) =
        findFirstDigit str,
        findLastDigit str

    let convertToNumber (d1: char, d2: char) =
        [|d1; d2|]
        |> String
        |> int

    let run inputFile =
        getInput inputFile
        |> Seq.map (findFirstAndLastDigits >> convertToNumber)
        |> Seq.sum

Solution.run "Part 1" Part1.run "Inputs/Actual/Day1.txt"
Solution.run "Part 2" Part2.run "Inputs/Actual/Day1.txt"
