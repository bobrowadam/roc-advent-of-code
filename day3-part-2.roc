app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.14.0/dC5ceT962N_4jmoyoffVdphJ_4GlW3YMhAPyGPr-nU0.tar.br" }

import pf.Stdout
import pf.Task
import pf.Utc
import "./day3-input.txt" as inputFile : Str

Item a := [Digit Str, Symbol, Empty, Star] implements [
        Eq { isEq: itemEquality },
    ]

itemEquality = \itemA, itemB ->
    when (itemA, itemB) is
        (@Item (Digit s1), @Item (Digit s2)) -> Bool.isEq s1 s2
        (@Item Empty, @Item Empty) -> Bool.true
        (@Item Symbol, @Item Symbol) -> Bool.true
        _ -> Bool.false

Matrix a : List (List (Item a))

main =
    start = Utc.now!
    sum = Num.toStr (schematicSum inputFile)
    end = Utc.now!
    totalRunTime = (Utc.deltaAsNanos end start) |> Num.toStr
    Task.await (Stdout.line "schematic sum: $(sum)") \_ ->
        Stdout.line! "Total run time: $(totalRunTime)"

schematicSum = \encodedSchematic ->
    calibrationValuesSum (decodedSchematic encodedSchematic)

calibrationValuesSum : Matrix a -> I64
calibrationValuesSum = \matrix ->
    List.walkWithIndex matrix 0 \yState, row, yIndex ->
        rowsToSum = splitMatrix matrix yIndex

        List.walkWithIndex row 0 \xState, item, xIndex ->
            when item is
                @Item Star ->
                    xState + (rowsMultiplication rowsToSum xIndex)

                _ -> xState
        |> \rowSum -> rowSum + yState

rowsMultiplication = \matrix, xIndex ->
    finalState = List.walk matrix [] \state, row ->
        numbersAdjacentToPointRes = numbersAdjacentToPoint row xIndex
        # if Num.absDiff starIndex rowIndex > 1 then
        #     []
        # else
        #     numbersAdjacentToPoint row xIndex

        List.concat state numbersAdjacentToPointRes

    when List.len finalState is
        2 ->
            when (List.first finalState, List.last finalState) is
                (Ok a, Ok b) -> a * b
                _ -> crash "Should not happen"

        _ -> 0

# splitMatrix : Matrix a, (I64 -> List (Item a)) -> List (Item a)
splitMatrix = \matrix, splitAtIndex ->
    matrixLen = List.len matrix
    if splitAtIndex == 0 then
        List.takeFirst matrix 2
    else if splitAtIndex == matrixLen - 1 then
        List.takeLast matrix 2
    else
        takeLast = (matrixLen - splitAtIndex + 1)

        List.takeLast matrix takeLast
        |> List.takeFirst 3

AccumulatedDigit : [Adjacent Str, NotAdjacent Str]

numbersAdjacentToPoint = \row, x ->
    zero : (List (List AccumulatedDigit), List AccumulatedDigit)
    zero = ([], [])

    List.walkWithIndexUntil row zero \(accNumbers, currentNumber), item, index ->
        if isAdjacentToX x index then
            when item is
                @Item (Digit d) ->
                    Continue (accNumbers, List.append currentNumber (Adjacent d))

                _ ->
                    Continue (List.append accNumbers currentNumber, [])
        else
            when item is
                @Item (Digit d) ->
                    Continue (accNumbers, List.append currentNumber (NotAdjacent d))

                _ ->
                    if (index > x) && (Num.absDiff index x >= 2) then
                        Break (List.append accNumbers currentNumber, [])
                    else
                        Continue (List.append accNumbers currentNumber, [])
    |> \(accNumbers, currentNumber) ->
        List.append accNumbers currentNumber
    |> List.keepIf isDigitListAdjacent
    |> List.mapTry mapAdjacentToNumber
    |> Result.withDefault []
    |> List.walk [] \flatten, number ->
        List.append flatten number

isDigitListAdjacent = \digitItemList ->
    List.findFirst digitItemList \digitItem ->
        when digitItem is
            Adjacent _ -> Bool.true
            _ -> Bool.false
    |> Result.isOk

isAdjacentToX : U64, U64 -> Bool
isAdjacentToX = \xPosition, x ->
    (Num.absDiff x xPosition) <= 1

mapAdjacentToNumber : List AccumulatedDigit -> Result I64 _
mapAdjacentToNumber = \adjacentList ->
    List.map adjacentList \adjacentDigit ->
        when adjacentDigit is
            Adjacent n -> n
            NotAdjacent n -> n
    |> Str.joinWith ""
    |> Str.toI64

encodedEngineSchematicLine = \encodedSchematicRow ->
    List.map encodedSchematicRow \encodedItem ->
        when encodedItem is
            @Item Star -> "*"
            @Item Empty -> "."
            @Item Symbol -> "+"
            @Item (Digit digitStr) -> digitStr
    |> List.append "\n"
    |> Str.joinWith ""

decodedSchematic = \encodedSchematic ->
    Str.split encodedSchematic "\n"
    |> List.walk [] \state, line ->
        decodedEngineSchematicLine line
        |> \decodedLineRes ->
            Result.map decodedLineRes (\decodedLine -> List.append state decodedLine)
        |> \stateRes ->
            when stateRes is
                Ok s -> s
                Err e -> crash "Can't decoded engine schematic. error: $(Inspect.toStr e)"

decodedEngineSchematicLine = \line ->
    Str.toUtf8 line
    |> List.map \u8 ->
        when u8 is
            48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 ->
                utf8Result = Str.fromUtf8 [u8]

                Result.map utf8Result \digitStr -> @Item (Digit digitStr)

            42 -> Ok (@Item Star)
            46 -> Ok (@Item Empty)
            33 | 35 | 36 | 37 | 43 | 45 | 47 | 61 | 64 | 94 | 38 | 40 | 41 -> Ok (@Item Symbol)
            c -> Err (U8 c)
    |> List.mapTry \itemResult -> itemResult
