#load "Utils.fsx"

open Utils

type Game<'Card> =
    { Hand: 'Card list
      Bid: int }

type HandType =
    | FiveKind
    | FourKind
    | FullHouse
    | ThreeKind
    | TwoPair
    | OnePair
    | HighCard

module HandType =
    let getWinningOrder =
        function
        | FiveKind -> 7
        | FourKind -> 6
        | FullHouse -> 5
        | ThreeKind -> 4
        | TwoPair -> 3
        | OnePair -> 2
        | HighCard -> 1


let getInput cardOfString inputFile =
    File.readStream inputFile
    |> Seq.map (fun line ->
        line
        |> Regex.matches "(\w){5} (\d+)"
        |> fun x ->
            { Hand =
                x[0].Groups[1].Captures
                |> List.ofSeq
                |> List.map (_.Value >> cardOfString)
              Bid = int x[0].Groups[2].Value })


module Part1 =
    type Card =
        | Two = 2
        | Three = 3
        | Four = 4
        | Five = 5
        | Six = 6
        | Seven = 7
        | Eight = 8
        | Nine = 9
        | Ten = 10
        | Jack = 11
        | Queen = 12
        | King = 13
        | Ace = 14

    module Card =
        let ofString =
            function
            | "A" -> Card.Ace
            | "K" -> Card.King
            | "Q" -> Card.Queen
            | "J" -> Card.Jack
            | "T" -> Card.Ten
            | x -> enum<Card> (int x)
            

    module HandType =
        let calculate (hand: Card list) =
            let groupTotals =
                hand
                |> List.groupBy id
                |> List.map (fun (_, xs) -> List.length xs)

            if groupTotals |> List.exists ((=) 5) then
                FiveKind
            elif groupTotals |> List.exists ((=) 4) then
                FourKind
            elif groupTotals |> List.exists ((=) 3) then
                if groupTotals |> List.exists ((=) 2) then
                    FullHouse
                else
                    ThreeKind
            elif groupTotals |> List.exists ((=) 2) then
                if groupTotals |> List.filter ((=) 2) |> List.length = 2 then
                    TwoPair
                else
                    OnePair
            else
                HighCard


    let run inputFile =
        getInput Card.ofString inputFile
        |> List.ofSeq
        |> List.sortBy _.Hand
        |> List.sortBy (_.Hand >> HandType.calculate >> HandType.getWinningOrder)
        |> List.mapi (fun idx game -> (idx + 1) * game.Bid)
        |> List.reduce (+)


// Cannot reuse as type values have changed :/ Only a pain because I want to keep a record of the old solution
module Part2 =
    type Card =
        | Joker = 1
        | Two = 2
        | Three = 3
        | Four = 4
        | Five = 5
        | Six = 6
        | Seven = 7
        | Eight = 8
        | Nine = 9
        | Ten = 10
        | Queen = 12
        | King = 13
        | Ace = 14

    module Card =
        let ofString =
            function
            | "A" -> Card.Ace
            | "K" -> Card.King
            | "Q" -> Card.Queen
            | "T" -> Card.Ten
            | "J" -> Card.Joker
            | x -> enum<Card> (int x)


    module HandType =
        let private getOptimumHand hand =
            let mostCommonCard =
                hand
                |> List.filter ((<>) Card.Joker)
                |> List.groupBy id
                |> function [] -> [ Card.Ace, [] ] | x -> x
                |> List.maxBy (snd >> List.length)
                |> fst

            hand
            |> List.map (function Card.Joker -> mostCommonCard | x -> x)

        let calculate (hand: Card list) =
            let groupTotals =
                getOptimumHand hand
                |> List.groupBy id
                |> List.map (fun (_, xs) -> List.length xs)

            if groupTotals |> List.exists ((=) 5) then
                FiveKind
            elif groupTotals |> List.exists ((=) 4) then
                FourKind
            elif groupTotals |> List.exists ((=) 3) then
                if groupTotals |> List.exists ((=) 2) then
                    FullHouse
                else
                    ThreeKind
            elif groupTotals |> List.exists ((=) 2) then
                if groupTotals |> List.filter ((=) 2) |> List.length = 2 then
                    TwoPair
                else
                    OnePair
            else
                HighCard


    let run inputFile =
        getInput Card.ofString inputFile
        |> List.ofSeq
        |> List.sortBy _.Hand
        |> List.sortBy (_.Hand >> HandType.calculate >> HandType.getWinningOrder)
        |> List.mapi (fun idx game -> (idx + 1) * game.Bid)
        |> List.reduce (+)


Solution.run "pt1" Part1.run  "Inputs/Actual/Day7.txt" // pt1 completed in 8ms with result: 253933213
Solution.run "pt2" Part2.run  "Inputs/Actual/Day7.txt" // pt2 completed in 11ms with result: 253473930