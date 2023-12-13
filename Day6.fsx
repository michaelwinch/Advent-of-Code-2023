#load "Utils.fsx"

open Utils

type Game =
    { Duration: int64
      RecordDistance: int64 }

let willWin game x =
    let distance = (game.Duration - x) * x
    distance > game.RecordDistance

let calculateWaysToWin (game: Game) =
    let options =
        seq { 1L .. game.Duration - 1L }

    let first = Seq.find (willWin game) options
    let last = Seq.findBack (willWin game) options

    last - first + 1L


module Part1 =
    let getInput inputFile =
        File.readStream inputFile
        |> Seq.fold (fun (times, distances as acc) ->
            function
            | Regex.Match "Time:(?:\s+(\d+))+" groups ->
                groups[1].Captures
                |> List.ofSeq
                |> List.map (_.Value >> int),
                distances
            | Regex.Match "Distance:(?:\s+(\d+))+" groups ->
                times,
                groups[1].Captures
                |> List.ofSeq
                |> List.map (_.Value >> int)
            | String.IsNullOrWhitespace -> acc
            | x -> failwithf "invalid input: %A" x) ([], [])
        ||> List.zip
        |> List.map (fun (time, distance) -> { Duration = time; RecordDistance = distance })

    let run inputFile =
        getInput inputFile
        |> List.map calculateWaysToWin
        |> List.reduce (*)

module Part2 =
    let getInput inputFile =
        File.readStream inputFile
        |> Seq.fold (fun (time, distance as acc) ->
            function
            | Regex.Match "Time:([\s\d]*)" groups ->
                groups[1].Value.Replace(" ", "")
                |> int64,
                distance
            | Regex.Match "Distance:([\s\d]*)" groups ->
                time,
                groups[1].Value.Replace(" ", "")
                |> int64
            | String.IsNullOrWhitespace -> acc
            | x -> failwithf "invalid input: %A" x) (0, 0)
        |> fun (time, distance) -> { Duration = time; RecordDistance = distance }

    let run inputFile =
        getInput inputFile
        |> calculateWaysToWin


Solution.run "pt1" Part1.run "Inputs/Actual/Day6.txt" // pt1 completed in 3ms with result: 4403592
Solution.run "pt2" Part2.run "Inputs/Actual/Day6.txt" // pt2 completed in 485ms with result: 38017587L