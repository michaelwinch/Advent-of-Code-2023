let tee f x = f x; x

let log x = printfn "LOG: %A" x
let logm m x = printfn "%s: %A" m x

module Solution =
    open System.Diagnostics

    let run name f input =
        let sw = Stopwatch.StartNew ()
        let result = f input
        let elapsedMs = sw.ElapsedMilliseconds
        printfn "%s completed in %dms with result: %A" name elapsedMs result


module File =
    open System.IO
    
    let readStream (inputFile: string) =
        seq {
            use streamReader = new StreamReader(inputFile)
            while not streamReader.EndOfStream do
                yield streamReader.ReadLine()
        }


module String =
    open System

    let (|IsNullOrWhitespace|_|) (x: string) =
        if String.IsNullOrWhiteSpace x then Some () else None


module Maths =
    let highestCommonFactor x y =
        let lower, higher = if x < y then x, y else y, x
        let rec loop lower higher =
            if lower = 0L then higher
            else
                loop (higher % lower) lower
        loop lower higher

    let lowestCommonMultiple x y =
        x * y / highestCommonFactor x y


module Regex =
    open System.Text.RegularExpressions

    let ``match`` pattern input =
        Regex.Match(input, pattern).Groups

    let matches pattern input =
        Regex.Matches(input, pattern)

    let (|Match|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some m.Groups
        else None

    let (|Matches|_|) pattern input =
        let m = Regex.Matches(input, pattern)
        if m.Count > 0 then Some m else None

    let (|MatchesAlways|_|) pattern input =
        let m = Regex.Matches(input, pattern)
        Some m


type Array2D<'a> = private Array2D of 'a array array

module Array2D =
    let init yLength xLength initialiser =
        [ 0 .. yLength - 1 ]
        |> List.map (fun y -> Array.init xLength (initialiser y))
        |> List.toArray
        |> Array2D

    let create (x: _ array array) =
        if x.Length = 0 || x |> Array.forall (_.Length >> (=) x[0].Length) then
            Array2D x
        else
            failwith "input array is jagged"

    let get y x (Array2D array) =
        array[y][x]

    let set (Array2D array) y x value =
        Array.set array[y] x value

    let findIndex predicate (Array2D array) =
        let y = Array.findIndex (Array.exists predicate) array
        let x = Array.findIndex predicate array[y]
        y, x

    let lengthY (Array2D array) =
        array.Length

    let lengthX (Array2D array) =
        array[0].Length

    let log (formatter: _ -> string) (Array2D array) =
        Array.map (Array.map formatter >> String.concat "") array
        |> Array.iter (printfn "%s")


module List =
    let getUniquePairs (xs: _ list) =
        let rec loop acc =
            function
            | [] -> acc
            | h :: t ->
                let newPairs = t |> List.map (fun t -> h, t)
                loop (newPairs @ acc) t

        loop [] xs


type List2D<'a> = 'a list list

module List2D =
    let create (x: _ list list) : _ List2D =
        if x.Length = 0 || x |> List.forall (_.Length >> (=) x[0].Length) then
            x
        else
            failwith "input list is jagged"

    let get y x (list: _ List2D) =
        list[y][x]

    let set y x value (list: _ List2D)=
        list
        |> List.mapi (fun idxY row ->
            if idxY = y then
                row
                |> List.mapi (fun idxX valX -> if idxX = x then value else valX)
            else
                row)

    let findIndex predicate (list: _ List2D) =
        let y = List.findIndex (List.exists predicate) list
        let x = List.findIndex predicate list[y]
        y, x

    let lengthY (list: _ List2D) =
        List.length list

    let lengthX (list: _ List2D) =
        List.length list[0]

    let mapi f =
        List.mapi (fun y row -> List.mapi (fun x value -> f y x value) row)

    let choose f =
        List.collect (List.choose f)

    let log (formatter: _ -> string) (list: _ List2D) =
        List.map (List.map formatter >> String.concat "") list
        |> List.iter log