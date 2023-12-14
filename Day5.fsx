#load "Utils.fsx"

open System
open Utils

type MapRange =
    { SourceStart: int64
      DestinationStart: int64
      Length: int64 }

module MapRange =
    let getSourceEnd (x: MapRange) =
        x.SourceStart + x.Length - 1L

    let getDestinationValue value map =
        (value - map.SourceStart) + map.DestinationStart


type DestinationMap =
    { SourceName: string
      DestinationName: string
      Ranges: MapRange list }

module DestinationMap =
    let getFromMap key destinationMap =
        destinationMap.Ranges
        |> List.tryFind (fun x -> x.SourceStart <= key && key <= MapRange.getSourceEnd x)
        |> Option.map (MapRange.getDestinationValue key)
        |> Option.defaultValue key
        |> fun value -> destinationMap.DestinationName, value


module Part1 =
    type ParseAcc =
        { Seeds: int64 list
          PartialMap: DestinationMap option
          CompletedMaps: DestinationMap list }

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

    let rec searchMaps (maps: Map<string, DestinationMap>) name value =
        match Map.tryFind name maps with
        | None -> value
        | Some map ->
            let destinationName, value = DestinationMap.getFromMap value map
            searchMaps maps destinationName value

    let run inputFile =
        let { Seeds = seeds; CompletedMaps = maps } =
            File.readStream inputFile
            |> List.ofSeq
            |> parseLines ParseAcc.empty

        let maps =
            maps
            |> List.map (fun x -> x.SourceName, x)
            |> Map

        seeds
        |> List.map (searchMaps maps "seed")
        |> List.min


module Part2 =
    type ValueRange =
        { Start: int64
          Length: int64 }

    module ValueRange =
        let getEnd (x: ValueRange) =
            x.Start + x.Length - 1L

    type ParseAcc =
        { Seeds: ValueRange list
          PartialMap: DestinationMap option
          CompletedMaps: DestinationMap list }

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
                |> List.chunkBySize 2
                |> List.map (fun x -> { Start = x[0]; Length = x[0] + x[1] - 1L })

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


    module MapRange =
        let (|AllValuesWithinMap|ValuesStartInMap|ValuesEndInMap|MapAllInValues|AllValuesOutsideMap|) (mapRange, valueRange) =
            if mapRange.SourceStart <= valueRange.Start
                && ValueRange.getEnd valueRange <= MapRange.getSourceEnd mapRange
            then AllValuesWithinMap
            elif mapRange.SourceStart <= valueRange.Start
                && valueRange.Start <= MapRange.getSourceEnd mapRange
                && MapRange.getSourceEnd mapRange < ValueRange.getEnd valueRange
            then ValuesStartInMap
            elif valueRange.Start < mapRange.SourceStart
                && mapRange.SourceStart <= ValueRange.getEnd valueRange
                && ValueRange.getEnd valueRange <= MapRange.getSourceEnd mapRange
            then ValuesEndInMap
            elif valueRange.Start <= mapRange.SourceStart
                && MapRange.getSourceEnd mapRange <= ValueRange.getEnd valueRange
            then MapAllInValues
            elif ValueRange.getEnd valueRange < mapRange.SourceStart
                || MapRange.getSourceEnd mapRange < valueRange.Start
            then AllValuesOutsideMap
            else failwithf "Case not handled; mapRange: %A valueRange %A" mapRange valueRange

        // let getValueRangesFromMap (valueRange: ValueRange) (mapRange: MapRange) =
        //     match mapRange, valueRange with
        //     | AllValuesWithinMap ->
        //         [ { valueRange with Start = MapRange.getDestinationValue valueRange.Start mapRange } ]
        //     | ValuesStartInMap ->
        //         let valuesInMap =
        //             { Start = MapRange.getDestinationValue valueRange.Start mapRange
        //               Range = MapRange.getSourceEnd mapRange - valueRange.Start + 1L }
        //
        //         let valuesOutsideMap =
        //             { Start = MapRange.getSourceEnd mapRange + 1L
        //               Range = ValueRange.getEnd valueRange - (MapRange.getSourceEnd mapRange + 1L) + 1L }
        //
        //         [ valuesInMap; valuesOutsideMap ]
        //     | ValuesEndInMap ->
        //         let valuesOutsideMap =
        //             { Start = valueRange.Start
        //               Range = (mapRange.SourceStart - 1L) - valueRange.Start + 1L }
        //
        //         let valuesInMap =
        //             { Start = mapRange.DestinationStart
        //               Range = ValueRange.getEnd valueRange - mapRange.SourceStart + 1L }
        //
        //         [ valuesOutsideMap; valuesInMap ]
        //     | AllValuesOutsideMap -> [ valueRange ]

        let getValueRangesFromMap (valueRange: ValueRange) (mapRange: MapRange) =
            match mapRange, valueRange with
            | AllValuesWithinMap ->
                Some valueRange
            | ValuesStartInMap ->
                { Start = valueRange.Start
                  Length = MapRange.getSourceEnd mapRange - valueRange.Start + 1L }
                |> Some
            | ValuesEndInMap ->
                { Start = mapRange.SourceStart
                  Length = ValueRange.getEnd valueRange - mapRange.SourceStart + 1L }
                |> Some
            | MapAllInValues ->
                { Start = mapRange.SourceStart
                  Length = mapRange.Length }
                |> Some
            | AllValuesOutsideMap ->
                None

    type RangeComparator =
        { Start: int64
          End: int64 }

    module RangeComparator =
        let (|AllValuesWithinMap|ValuesStartInMap|ValuesEndInMap|MapAllInValues|AllValuesOutsideMap|) (mapRange, valueRange) =
            if mapRange.Start <= valueRange.Start
                && valueRange.End <= mapRange.End
            then AllValuesWithinMap
            elif mapRange.Start <= valueRange.Start
                && valueRange.Start <= mapRange.End
                && mapRange.End < valueRange.End
            then ValuesStartInMap
            elif valueRange.Start < mapRange.Start
                && mapRange.Start <= valueRange.End
                && valueRange.End <= mapRange.End
            then ValuesEndInMap
            elif valueRange.Start <= mapRange.Start
                && mapRange.End <= valueRange.End
            then MapAllInValues
            elif valueRange.End < mapRange.Start
                || mapRange.End < valueRange.Start
            then AllValuesOutsideMap
            else failwithf "Case not handled; mapRange: %A valueRange %A" mapRange valueRange

    let getValueRangesWithNoMatches valueRange matches =
        let rec loop (valueRange, nonMatchedRanges) (matches: (RangeComparator * RangeComparator) list) =
            match matches with
            // | AllValuesWithinMap as mapRange :: rest ->
            //     let valueRange =
            //         { Start = mapRange.End
            //           End = valueRange.End }
            //     loop (valueRange, nonMatchedRanges) rest
            | ValuesStartInMap as mapRange :: rest ->
                let valueRange =
                    { Start = mapRange.End
                      End = valueRange.End }
                loop (valueRange, nonMatchedRanges) rest
            | ValuesEndInMap as mapRange :: rest ->
                let valueRange =
                    { Start = mapRange.End
                      End = valueRange.End }
                loop (valueRange, nonMatchedRanges) rest


        matches
        |> List.sortBy (fun x -> x.Start)
        |> loop (valueRange, [])

    let rec searchMaps (maps: Map<string, DestinationMap>) name valueRange =
        match Map.tryFind name maps with
        | None -> [ valueRange ]
        | Some map ->
            let matches =
                map.Ranges
                |> List.choose (fun x -> MapRange.getValueRangesFromMap valueRange x |> Option.map (fun y -> x, y))

            let valueRangesWithNoMatches =
                matches
                |> List.map snd
                |> getValueRangesWithNoMatches valueRange

            let valueRangesWithMatches =
                matches
                |> List.map (fun (mapRange, valueRange) ->
                    { valueRange with Start = MapRange.getDestinationValue valueRange.Start mapRange })

            (valueRangesWithNoMatches @ valueRangesWithMatches)
            |> List.collect (searchMaps maps map.DestinationName)

    let run inputFile =
        let { Seeds = seeds; CompletedMaps = maps } =
            File.readStream inputFile
            |> List.ofSeq
            |> parseLines ParseAcc.empty

        let maps =
            maps
            |> List.map (fun x -> x.SourceName, x)
            |> Map

        seeds
        |> List.collect (searchMaps maps "seed")
        |> List.min


//Solution.run "pt1" Part1.run "Inputs/Actual/Day5.txt" // pt1 completed in 4ms with result: 107430936
Solution.run "pt2" Part2.run "Inputs/Examples/Day5.txt"