app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.12.0/Lb8EgiejTUzbggO2HVVuPJFkwvvsfW6LojkLR20kTVE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.7.1/MvLlME9RxOBjl0QCxyn3LIaoG9pSlaNxCa-t3BfbPNc.tar.br",
}
import pf.Task
import pf.Stdout
import "./day3-input.txt" as inputFile : Str
import pf.Utc

Item a := [Digit Str, Symbol, Empty] implements [
        Eq { isEq: itemEquality },
    ]

itemEquality = \itemA, itemB ->
    dbg itemA
    dbg itemB
    when (itemA, itemB) is
        (@Item (Digit s1), @Item (Digit s2)) -> (Bool.isEq s1 s2)
        (@Item Empty, @Item Empty ) -> Bool.true
        (@Item Symbol, @Item Symbol ) -> Bool.true
        _ -> Bool.false

main =
    startTime = Utc.now!
    List.walk (Str.split inputFile "\n") [] \matrix, line ->
        engineSchematicLine = parseEngineSchematicLine line
        when engineSchematicLine is
            Ok l -> List.append matrix l
            Err e -> crash "Error in parse: $(Inspect.toStr e)"
    |> matrixSum
    |> \res ->
           Stdout.line! "Done: $(Inspect.toStr res)"
           runTime = (Utc.deltaAsMillis Utc.now! startTime)
           Stdout.line! "Total run time: $(Inspect.toStr runTime)"

parsedTestInput : List (List (Item a))
parsedTestInput = [
    [@Item Empty, @Item Empty, @Item Symbol],
    [@Item (Digit "4"), @Item (Digit "6"), @Item (Digit "7")],
    [@Item Empty, @Item Empty, @Item Empty],
    [@Item (Digit "3"), @Item Empty, @Item (Digit "7")],
    [@Item Symbol, @Item Empty, @Item Empty],
]

matrixSum = \matrix ->
    List.walkWithIndex matrix 0 \sum, row, yIndex ->
        sum + (rowSum row yIndex matrix)

expect
    res = matrixSum parsedTestInput
    res == 470

rowSum = \row, y, matrix ->
    markAdjacent = \digit, x ->
        if (symbolAdjacent matrix (x, y)) then
            Adjacent digit
        else
            NotAdjacent digit
    strippedDigits = \maybeAdjacentDigits ->
        List.map maybeAdjacentDigits \taggedDigit ->
            when taggedDigit is
                Adjacent d -> d
                NotAdjacent d -> d

    sumOfDigits : List [Adjacent Str, NotAdjacent Str] -> U64
    sumOfDigits = \maybeAdjacentDigits ->
        adjacentFound = List.findFirst maybeAdjacentDigits \d ->
            when d is
                Adjacent _ -> Bool.true
                NotAdjacent _ -> Bool.false
        when adjacentFound is
            Ok _ -> Result.withDefault
                (strippedDigits maybeAdjacentDigits
                 |> Str.joinWith ""
                 |> Str.toU64)
                 0
            Err _ -> 0

    List.walkWithIndex row ([], 0) \(digits, sum), item, index ->
        when item is
            @Item (Digit n) -> (List.append digits (markAdjacent n index), sum)
            @Item _ -> ([], sum + when digits is
                [..] -> (sumOfDigits digits)
                _ -> 0)
    |> \(digits, sum) -> (sumOfDigits digits) + sum

expect
    testedRow = 4
    row = List.get parsedTestInput testedRow |> Result.withDefault []
    res = rowSum row testedRow parsedTestInput
    res == 0

expect
    testedRow = 3
    row = List.get parsedTestInput testedRow |> Result.withDefault []
    res = rowSum row testedRow parsedTestInput
    res == 3

symbolAdjacent = \matrix, (xi, yi) ->
    (xStartAt, xLength) = if Num.isGt xi 0 then
        (xi - 1, 3)
        else (xi, 2)
    (yStartAt, yLength) = if Num.isGt yi 0 then
        (yi - 1, 3)
        else (yi, 2)

    xs = List.range { start: At xStartAt, end: Length xLength }
    ys = List.range { start: At yStartAt, end: Length yLength }

    List.findFirst ys \y ->
        when List.get matrix y is
            Ok row -> symbolInRow xs row
            Err _ -> Bool.false
    |> Result.map \_ -> Bool.true
    |> Result.mapErr \_ -> Bool.false
    |> \result ->
        when result is
            Ok _ -> Bool.true
            Err _ -> Bool.false

expect
    test1 = symbolAdjacent parsedTestInput (2, 1)
    test1 == Bool.true

expect
    test2 = symbolAdjacent parsedTestInput (1, 1)
    test2 == Bool.true

expect
    test3 = symbolAdjacent parsedTestInput (0, 1)
    test3 == Bool.false

expect
    test3 = symbolAdjacent parsedTestInput (0, 3)
    test3 == Bool.true

symbolInRow = \xs, row ->
    List.findFirst xs \x ->
        List.get row x
        |> \r -> when r is
            Ok (@Item Symbol) -> Bool.true
            _ -> Bool.false
    |> Result.isOk

parseEngineSchematicLine = \line ->
    Str.toUtf8 line
    |> List.map \u8 ->
           dbg u8
           when u8 is
               48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 ->
                   utf8Result = Str.fromUtf8 [u8]
                   dbg utf8Result
                   Result.map utf8Result \digitStr -> @Item (Digit digitStr)
               46 -> Ok (@Item Empty)
               33 | 35 | 36 | 37 | 43 | 45 | 47 | 61 | 64 | 94 | 38 | 42 | 40 | 41 -> Ok (@Item Symbol)
               c -> Err (U8 c)
    |> List.mapTry \itemResult -> itemResult

expect
    res = Str.split rawTestInput "\n"
        |> List.first
        |> Result.try \line -> parseEngineSchematicLine line

    expected = when res is
        Ok l -> l
        _ -> []

    expected == [
        @Item (Digit "4"), @Item (Digit "6"), @Item (Digit "7"),
        @Item Empty, @Item Empty, @Item Empty, @Item Empty,
        @Item Empty, @Item Empty, @Item Empty
    ]

rawTestInput =
    """
    467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..
    """
