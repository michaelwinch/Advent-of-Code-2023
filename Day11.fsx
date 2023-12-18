#load "./Utils.fsx"

open System
open Utils

type Data =
    | Empty
    | Galaxy

module Data =
    let ofChar =
        function
        | '.' -> Empty
        | '#' -> Galaxy
        | x -> failwithf "invalid data: %c" x


let parseData (input: string seq) =
    input
    |> Array.ofSeq
    |> Array.map (_.ToCharArray() >> List.ofArray)
    |> List.ofArray
    |> List.map (List.map Data.ofChar)
    |> List2D.create

let isRowEmpty = List.forall ((=) Empty)


module Part1 =
    let expandSpaces (image: Data List2D) =
        let expandRows = List.collect (fun row -> if isRowEmpty row then [ row; row ] else [ row ])

        image
        |> expandRows
        |> List.transpose
        |> expandRows

    let getDistance (y1: int, x1: int) (y2, x2) =
        Math.Abs (y1 - y2) + Math.Abs (x1 - x2)

    let run inputFile =
        let image =
            File.readStream inputFile
            |> parseData
            |> expandSpaces

        let galaxyCoords =
            image
            |> List2D.mapi (fun y x -> function Galaxy -> Some (y, x) | _ -> None)
            |> List2D.choose id


        List.getUniquePairs galaxyCoords
        |> List.map (fun (v1, v2) -> getDistance v1 v2)
        |> List.reduce (+)


module Part2 =
    let getDistance
        (expansionDistance: int)
        (emptyRowIndexes: int list)
        (emptyColumnIndexes: int list)
        (y1: int, x1: int)
        (y2: int, x2: int)
        =
        let lowerY, higherY = if y1 < y2 then y1, y2 else y2, y1
        let lowerX, higherX = if x1 < x2 then x1, x2 else x2, x1

        let expandedSpaces =
            let expandedRows =
                emptyRowIndexes
                |> List.filter (fun y -> lowerY < y && y < higherY)
                |> List.length
            
            let expandedColumns =
                emptyColumnIndexes
                |> List.filter (fun x -> lowerX < x && x < higherX)
                |> List.length

            expandedRows + expandedColumns

        let (!) x = int64 x
        (!higherY - !lowerY) + (!higherX - !lowerX) + ((!expansionDistance - 1L) * !expandedSpaces)

    let run inputFile =
        let image =
            File.readStream inputFile
            |> parseData

        let emptyRowIndexes =
            image
            |> List.mapi (fun y row -> if isRowEmpty row then Some y else None)
            |> List.choose id

        let emptyColumnIndexes =
            image
            |> List.transpose
            |> List.mapi (fun x row -> if isRowEmpty row then Some x else None)
            |> List.choose id

        let galaxyCoords =
            image
            |> List2D.mapi (fun y x -> function Galaxy -> Some (y, x) | _ -> None)
            |> List2D.choose id

        List.getUniquePairs galaxyCoords
        |> List.map (fun (v1, v2) -> getDistance 1000000 emptyRowIndexes emptyColumnIndexes v1 v2)
        |> List.reduce (+)


Solution.run "pt1" Part1.run "Inputs/Actual/Day11.txt" // pt1 completed in 12ms with result: 9233514
Solution.run "pt2" Part2.run "Inputs/Actual/Day11.txt" // pt2 completed in 21ms with result: 363293506944