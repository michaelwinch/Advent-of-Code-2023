#load "./Utils.fsx"

open System
open Utils

type Move =
    | Right
    | Down
    | Left
    | Up


type Array2D<'a> = private Array2D of 'a array array

module Array2D =
    let create (x: _ array array) =
        if x.Length = 0 || x |> Array.forall (_.Length >> (=) x[0].Length) then
            Array2D x
        else
            failwith "input array is jagged"

    let get y x (Array2D array) =
        array[y][x]

    let find predicate (Array2D array) =
        let y = Array.findIndex (Array.exists predicate) array
        let x = Array.findIndex predicate array[y]
        y, x
            

let getInput inputFile =
    File.readStream inputFile
    |> Array.ofSeq
    |> Array.map _.ToCharArray()
    |> Array2D.create

let getFirstMove (starty, startx) map =
    [ yield Right, starty, (startx + 1), ['-'; 'J'; '7']
      yield Down, (starty + 1), startx, ['|'; 'L'; 'J']

      if startx <> 0 then
        yield Left, starty, (startx - 1), ['-'; 'F'; 'L']

      if starty <> 0 then
        yield Up, (starty - 1), startx, ['|'; '7'; 'F'] ]
    |> List.find (fun (_, y, x, options) -> List.contains (Array2D.get y x map) options)
    |> fun (move, y, x, _) -> move, (y, x)

let getNextMove (currentMove, (y, x)) currentValue =
    let newMove =
        match currentMove, currentValue with
        | Right, '-' -> Right
        | Right, 'J' -> Up
        | Right, '7' -> Down
        | Down, '|' -> Down
        | Down, 'J' -> Left
        | Down, 'L' -> Right
        | Left, '-' -> Left
        | Left, 'L' -> Up
        | Left, 'F' -> Down
        | Up, '|' -> Up
        | Up, '7' -> Left
        | Up, 'F' -> Right
        | _ ->
            failwithf "invalid part of loop, last move: %A, current value %c, current coords (%d, %d)"
                currentMove
                currentValue
                y x

    let newCoords =
        match newMove with
        | Right -> y, x + 1
        | Down -> y + 1, x
        | Left -> y, x - 1
        | Up -> y - 1, x

    newMove, newCoords

let traverse (map: char Array2D) =
    let start = map |> Array2D.find ((=) 'S')
    let firstMove = getFirstMove start map
    
    let rec loop moves (_, (y, x) as currentAction) =
        let moves = moves + 1
        let currentValue = Array2D.get y x map

        if currentValue = 'S' then
            moves
        else
            let nextMove = getNextMove currentAction currentValue
            loop moves nextMove

    let loopLength = loop 0 firstMove
    loopLength / 2


module Part1 =
    let run inputFile =
        getInput inputFile
        |> traverse


Solution.run "pt1" Part1.run "Inputs/Actual/Day10.txt" // pt1 completed in 2ms with result: 6613