#load "./Utils.fsx"

open System
open Utils

let parseLine (line: string) =
    line.Split(' ')
    |> List.ofArray
    |> List.filter (String.IsNullOrEmpty >> not)
    |> List.map int64

let rec calculateDifferences stacc =
    let nextLevelDifferences =
        stacc
        |> List.head
        |> List.windowed 2
        |> List.map (List.rev >> List.reduce (-))

    let stacc = nextLevelDifferences :: stacc

    if nextLevelDifferences |> List.forall ((=) 0L) then
        stacc
    else
        calculateDifferences stacc


module Part1 =
    let rec calculateNextValues =
        function
        | [] -> failwith "no input values"
        | values :: [] -> List.last values
        | values1 :: values2 :: t ->
            let next = List.last values1 + List.last values2
            calculateNextValues ((values2 @ [next]) :: t)

    let run inputFile =
        File.readStream inputFile
        |> Seq.map parseLine
        |> Seq.map (List.singleton >> calculateDifferences >> calculateNextValues)
        |> Seq.reduce (+)


module Part2 =
    let rec calculatePreviousValues =
        function
        | values :: [] -> List.head values
        | (x :: _) :: (y :: _ as values2) :: t ->
            let next = y - x
            calculatePreviousValues ((next :: values2) :: t)
        | _ -> failwith "invalid input values"

    let run inputFile =
        File.readStream inputFile
        |> Seq.map parseLine
        |> Seq.map (List.singleton >> calculateDifferences >> calculatePreviousValues)
        |> Seq.reduce (+)


Solution.run "pt1" Part1.run "Inputs/Actual/Day9.txt" // pt1 completed in 9ms with result: 2175229206L
Solution.run "pt2" Part2.run "Inputs/Actual/Day9.txt" // pt2 completed in 4ms with result: 942L