#load "./Utils.fsx"

open Utils

type Instruction =
    | Left
    | Right

module Instruction =
    let ofString =
        function
        | "L" -> Left
        | "R" -> Right
        | x -> failwithf "invalid instruction: %s" x


let getInput inputFile =
    File.readStream inputFile
    |> Seq.fold (fun ((instructions, nodes) as acc) ->
        function
        | Regex.Match "^(R|L)+$" groups ->
            groups[1].Captures
            |> List.ofSeq
            |> List.map (_.Value >> Instruction.ofString),
            nodes
        | Regex.Matches "(\w+) = \((\w+), (\w+)\)" matches ->
            let key = matches[0].Groups[1].Value
            let leftChoice = matches[0].Groups[2].Value
            let rightChoice = matches[0].Groups[3].Value
            instructions, Map.add key (leftChoice, rightChoice) nodes
        | String.IsNullOrWhitespace -> acc
        | x -> failwithf "invalid input: %A" x) ([], Map [])

let traverseNodes endCondition instructions nodes startKey =
    let rec loop (steps, nodeKey as acc) =
        function
        | [] -> loop acc instructions
        | instruction :: t ->
            let newNodeKey =
                Map.find nodeKey nodes
                |> fun (leftKey, rightKey) ->
                        if instruction = Left then leftKey
                        else rightKey

            let steps = steps + 1L

            if endCondition newNodeKey then steps
            else
                loop (steps, newNodeKey) t

    loop (0L, startKey) instructions


module Part1 =
    let [<Literal>] startKey = "AAA"
    let [<Literal>] endKey = "ZZZ"
        
    let run inputFile =
        let instructions, nodes = getInput inputFile
        traverseNodes ((=) endKey) instructions nodes startKey
        

module Part2 =
    let getStartKeys (nodes: Map<string, string * string>) =
        nodes
        |> List.ofSeq
        |> List.map _.Key
        |> List.filter _.EndsWith("A")

    let run inputFile =
        let instructions, nodes = getInput inputFile
        getStartKeys nodes
        |> List.map (traverseNodes (_.EndsWith("Z")) instructions nodes)
        |> List.reduce Maths.lowestCommonMultiple


Solution.run "pt1" Part1.run "Inputs/Actual/Day8.txt" // pt1 completed in 5ms with result: 14257
Solution.run "pt2" Part2.run "Inputs/Actual/Day8.txt" // pt2 completed in 15ms with result: 16187743689077