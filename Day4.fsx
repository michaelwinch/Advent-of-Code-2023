#load "Utils.fsx"

open Utils

type Card =
    { WinningNumbers: int list
      FoundNumbers: int list }

module Card =
    let getNumberOfMatches (card: Card) =
        card.FoundNumbers
        |> List.filter (fun x -> card.WinningNumbers |> List.contains x)
        |> List.length

    let getScore (card: Card) =
        let matches = getNumberOfMatches card

        if matches = 0 then 0
        else System.Math.Pow(2, float (matches - 1)) |> int

    let parse str =
        let matches = Regex.matches "Card\s+(\d+):(?:\s+(\d+))+ \|(?:\s+(\d+))+" str
        { WinningNumbers =
            matches[0].Groups[2].Captures
            |> List.ofSeq
            |> List.map (fun x -> int x.Value)
          FoundNumbers =
            matches[0].Groups[3].Captures
            |> List.ofSeq
            |> List.map (fun x -> int x.Value) }


let getCards inputFile =
    File.readStream inputFile
    |> Seq.map Card.parse

module Part1 =
    let run inputFile =
        getCards inputFile
        |> Seq.sumBy Card.getScore


module Part2 =
    type CardWithCount =
        { Card: Card
          Count: int }

    module CardWithCount =
        let ofCard card =
            { Card = card
              Count = 1 }

        let copyCards numberOfCopies card =
            { card with Count = card.Count + numberOfCopies }


    let rec private loop (acc: CardWithCount list) =
        function
        | [] -> acc
        | card :: remainingCards ->
            let numberOfMatches = Card.getNumberOfMatches card.Card

            let copiedCards =
                remainingCards
                |> List.take numberOfMatches
                |> List.map (CardWithCount.copyCards card.Count)

            copiedCards @ List.skip numberOfMatches remainingCards
            |> loop (card :: acc)

    let run inputFile =
        getCards inputFile
        |> List.ofSeq
        |> List.map CardWithCount.ofCard
        |> loop []
        |> List.sumBy (fun x -> x.Count)
        

Solution.run "pt1" Part1.run "Inputs/Actual/Day4.txt" // pt1 completed in 2ms with result: 26346
Solution.run "pt2" Part2.run "Inputs/Actual/Day4.txt" // pt2 completed in 29ms with result: 8467762
