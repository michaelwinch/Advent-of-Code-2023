#load "./Utils.fsx"

open System.IO
open Utils

type Cubes =
    { Red: int
      Green: int
      Blue: int }

type Game =
    { Id: int
      Subsets: Cubes list }

let getInput (inputFile: string) =
    seq {
        use streamReader = new StreamReader(inputFile)
        while not streamReader.EndOfStream do
            yield streamReader.ReadLine()
    }

let parseSubset (str: string) =
    let getCount colour =
        let expression = $"(\d+) {colour}"
        match str with
        | Regex.Match expression groups -> int groups[1].Value
        | _ -> 0

    { Red = getCount "red"
      Green = getCount "green"
      Blue = getCount "blue" }

let parseGame (str: string) =
    let id, subsets =
        Regex.``match`` "Game (\d+): (.*)" str
        |> fun groups -> int groups[1].Value, groups[2].Value

    { Id = id
      Subsets =
          subsets.Split ";"
          |> List.ofSeq
          |> List.map parseSubset }


module Part1 =
    let testCase =
        { Red = 12
          Green = 13
          Blue = 14 }

    let isPossible testCase game =
        let maxCubes =
            let getMax f = game.Subsets |> List.map f |> List.max
            { Red = getMax (fun x -> x.Red)
              Green = getMax (fun x -> x.Green)
              Blue = getMax (fun x -> x.Blue) }

        maxCubes.Red <= testCase.Red
        && maxCubes.Green <= testCase.Green
        && maxCubes.Blue <= testCase.Blue

    let run inputFile =
        getInput inputFile
        |> Seq.map parseGame
        |> Seq.filter (isPossible testCase)
        |> Seq.sumBy (fun x -> x.Id)


module Part2 =
    let getMinimumCubes game =
        let getMax f = game.Subsets |> List.map f |> List.max
        { Red = getMax (fun x -> x.Red)
          Green = getMax (fun x -> x.Green)
          Blue = getMax (fun x -> x.Blue) }

    let calculatePower cubes =
        cubes.Red * cubes.Green * cubes.Blue

    let run inputFile =
        getInput inputFile
        |> Seq.map (parseGame >> getMinimumCubes)
        |> Seq.sumBy calculatePower


Solution.run "Part 1" Part1.run "Inputs/Actual/Day2.txt"
Solution.run "Part 2" Part2.run "Inputs/Actual/Day2.txt"