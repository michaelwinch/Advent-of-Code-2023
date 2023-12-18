#load "./Utils.fsx"

open System
open Utils

type Move =
    | Right
    | Down
    | Left
    | Up

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

let getLoopLength (map: char Array2D) =
    let start = map |> Array2D.findIndex ((=) 'S')
    let firstMove = getFirstMove start map
    
    let rec loop moves (_, (y, x) as currentAction) =
        let moves = moves + 1
        let currentValue = Array2D.get y x map

        if currentValue = 'S' then
            moves
        else
            let nextMove = getNextMove currentAction currentValue
            loop moves nextMove

    loop 0 firstMove


module Part1 =
    let run inputFile =
        getInput inputFile
        |> getLoopLength
        |> fun x -> x / 2


module Part2 =
    let getPipeLoopMap map =
        let pipeLoopMap = Array2D.init (Array2D.lengthY map) (Array2D.lengthX map) (fun _ _ -> false)

        let start = map |> Array2D.findIndex ((=) 'S')

        let firstMove = getFirstMove start map
    
        let rec loop (_, (y, x) as currentAction) =
            Array2D.set pipeLoopMap y x true
            let currentValue = Array2D.get y x map

            if currentValue = 'S' then
                ()
            else
                let nextMove = getNextMove currentAction currentValue
                loop nextMove

        loop firstMove

        pipeLoopMap

    let getSurroundingMoves (starty, startx) map =
        [ if startx + 1 <> Array2D.lengthX map then
              yield Right, starty, (startx + 1)

          if starty + 1 <> Array2D.lengthY map then
              yield Down, (starty + 1), startx

          if startx <> 0 then
              yield Left, starty, (startx - 1)

          if starty <> 0 then
              yield Up, (starty - 1), startx ]
        |> List.map (fun (move, y, x) -> move, (y, x))

    let floodFill pipeMap =
        let floodMap = Array2D.init (Array2D.lengthY pipeMap) (Array2D.lengthX pipeMap) (fun _ _ -> None)

        let rec loop isInsideLoop lastValuePipe (y, x) =
            let isCurrentPipe = Array2D.get y x pipeMap
            let surroundingLocations = getSurroundingMoves (y, x) pipeMap
            Array2D.set floodMap y x (Some (isInsideLoop && not isCurrentPipe))
            surroundingLocations
            |> List.filter (fun (_, (y, x)) -> Array2D.get y x floodMap |> Option.isNone)
            |> List.iter (snd >> loop (if lastValuePipe && isCurrentPipe then not isInsideLoop else isInsideLoop) isCurrentPipe)

        loop false false (0, 0)

        floodMap
        //|> Array2D.log (function None -> "X" | Some true -> "o" | Some false -> "-")

    let run inputFile =
        getInput inputFile
        |> getPipeLoopMap
        |> tee (Array2D.log (function true -> "*" | false -> "."))
        |> floodFill
        |> tee (Array2D.log (function None -> "X" | Some true -> "o" | Some false -> "-"))


//Solution.run "pt1" Part1.run "Inputs/Actual/Day10.txt" // pt1 completed in 2ms with result: 6613

Part2.run "Inputs/Examples/Day10_1_2.txt"
//|> Array2D.log (function true -> "*" | false -> ".")
//|> printfn "%A"