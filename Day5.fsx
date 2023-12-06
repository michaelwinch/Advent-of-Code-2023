#load "Utils.fsx"

open Utils

type MapRange =
    { SourceStart: int64
      DestinationStart: int64
      Length: int64 }

type PartialMap =
    { SourceName: string
      DestinationName: string
      Ranges: MapRange list }

type CompleteMap =
    { SourceName: string
      DestinationName: string
      Map: Map<int64, int64> }

module CompleteMap =
    let getFromMap key completeMap =
        completeMap.Map
        |> Map.tryFind key
        |> Option.defaultValue key
        |> fun value -> completeMap.DestinationName, value

    let ofPartialMap (partialMap: PartialMap) : CompleteMap =
        { SourceName = partialMap.SourceName
          DestinationName = partialMap.DestinationName
          Map =
            partialMap.Ranges
            |> List.collect (fun range ->
                List.zip
                    [range.SourceStart .. range.SourceStart + range.Length]
                    [range.DestinationStart .. range.DestinationStart + range.Length])
            |> Map }

type ParseAcc =
    { Seeds: int64 list
      PartialMap: PartialMap option
      CompletedMaps: PartialMap list }

module ParseAcc =
    let empty =
        { Seeds = []
          PartialMap = None
          CompletedMaps = [] }


let rec parseLines (acc: ParseAcc) =
    function
    | [] ->
        let completedMaps =
            match acc.PartialMap with
            | Some x -> x :: acc.CompletedMaps
            | None -> acc.CompletedMaps

        { acc with CompletedMaps = completedMaps }
    | String.IsNullOrWhitespace :: remaining ->
        parseLines acc remaining
    | Regex.Matches "seeds:(?:\s+(\d+))+" matches :: remaining ->
        let seeds =
            matches[0].Groups[1].Captures
            |> List.ofSeq
            |> List.map (fun x -> int64 x.Value)
        parseLines { acc with Seeds = seeds } remaining
    | Regex.Matches "(\w+)\-to\-(\w+) map:" matches :: remaining ->
        let completedMaps =
            match acc.PartialMap with
            | Some x -> x :: acc.CompletedMaps
            | None -> acc.CompletedMaps

        let newMap =
            { SourceName = matches[0].Groups[1].Captures[0].Value
              DestinationName = matches[0].Groups[2].Captures[0].Value
              Ranges = [] }

        parseLines { acc with PartialMap = Some newMap; CompletedMaps =  completedMaps } remaining
    | Regex.Matches "(\d+)\s+(\d+)\s+(\d+)" matches :: remaining ->
        let newRange =
            { SourceStart = int64 matches[0].Groups[2].Captures[0].Value
              DestinationStart = int64 matches[0].Groups[1].Captures[0].Value
              Length = int64 matches[0].Groups[3].Captures[0].Value }

        let partialMap =
            match acc.PartialMap with
            | Some partialMap ->
                { partialMap with Ranges = newRange :: partialMap.Ranges }
            | None ->
                failwith "trying to create new map range but no partial map exists"

        parseLines { acc with PartialMap = Some partialMap } remaining
    | x :: _ -> failwithf "unexpected line: %s" x


module Part1 =
    let rec searchMaps (maps: Map<string, CompleteMap>) name value =
        match Map.tryFind name maps with
        | None -> value
        | Some map ->
            let destinationName, value = CompleteMap.getFromMap value map
            searchMaps maps destinationName value

    let run inputFile =
        let { Seeds = seeds; CompletedMaps = partialMaps } =
            File.readStream inputFile
            |> List.ofSeq
            |> parseLines ParseAcc.empty

        let completedMaps =
            partialMaps
            |> List.map CompleteMap.ofPartialMap
            |> List.map (fun x -> x.SourceName, x)
            |> Map

        seeds
        |> List.map (searchMaps completedMaps "seed")
        |> List.min


Solution.run "pt1" Part1.run "Inputs/Actual/Day5.txt"
