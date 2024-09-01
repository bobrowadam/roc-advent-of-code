app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
}

import pf.Stdout
import pf.Utc

main =
    start = Utc.now! {}
    # sum = Num.toStr (schematicSum inputFile)
    end = Utc.now! {}
    totalRunTime = (Utc.deltaAsNanos end start) |> Num.toStr
    Task.await (Stdout.line "solving") \_ ->
        Stdout.line! "Total run time: $(totalRunTime)"

Card : { winningNumbers : List U64, numbersWeGot : List U64 }

testInput =
    """
    Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
    """

decodedScratchcardsInput : Str -> List Card
decodedScratchcardsInput = \encodedScratchCards ->
    Str.split testInput "\n"
    |> List.map \line -> Str.split line ":"
    |> List.mapTry List.last
    |> List.map \str -> Result.map  str \str1 -> (Str.split str1 "|")
    |> List.map \([firstStr, lastStr]) ->
         {
             winningNumbers : (parseNumbersStr firstStr),
             numbersWeGot : (parseNumbersStr lastStr)
         }
    # |> Result.withDefault (crash "")

parseNumbersStr = \numbersStr ->
    Str.split numbersStr " "
    |> List.keepOks Str.toU64

expect
    res = decodedScratchcardsInput testInput
    res
    == [
        { winningNumbers: [41, 48, 83, 86, 17], numbersWeGot: [83, 86, 6, 31, 17, 9, 48, 53] },
        { winningNumbers: [13, 32, 20, 16, 61], numbersWeGot: [61, 30, 68, 82, 17, 32, 24, 19] },
        { winningNumbers: [1, 21, 53, 59, 44], numbersWeGot: [69, 82, 63, 72, 16, 21, 14, 1] },
        { winningNumbers: [41, 92, 73, 84, 69], numbersWeGot: [59, 84, 76, 51, 58, 5, 54, 83] },
        { winningNumbers: [87, 83, 26, 28, 32], numbersWeGot: [88, 30, 70, 12, 93, 22, 82, 36] },
        { winningNumbers: [31, 18, 13, 56, 72], numbersWeGot: [74, 77, 10, 23, 35, 67, 36, 11] },
    ]

cardScore = \winningNumbers, numbersWeGot ->
    List.keepIf winningNumbers \winningNumber ->
        Result.isOk (List.findFirst numbersWeGot \numberWeGot -> numberWeGot == winningNumber)
    |> List.len
    |> Num.toFrac
    |> \length -> Num.pow 2 (length - 1)
    |> Num.round

expect
    res = cardScore [41, 48, 83, 86, 17] [83, 86, 6, 31, 17, 9, 48, 53]
    res == 8

expect
    res = cardScore [13, 32, 20, 16, 61] [61, 30, 68, 82, 17, 32, 24, 19]
    res == 2

expect
    res = cardScore [1, 21, 53, 59, 44] [69, 82, 63, 72, 16, 21, 14, 1]
    res == 2
