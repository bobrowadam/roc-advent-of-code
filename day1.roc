app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.12.0/Lb8EgiejTUzbggO2HVVuPJFkwvvsfW6LojkLR20kTVE.tar.br",
}
import pf.Stdout
import pf.Task
import pf.UTC
import "./input.txt" as inputFile : Str

main =
    startTime = UTC.now!
    dbg startTime

    parsedLines = splitLines inputFile
    res = calculateValuesSum parsedLines
    endTime = UTC.now!
    dbg endTime

    runTime = UTC.deltaAsMillis endTime startTime

    Stdout.line! "finished with: $(Inspect.toStr res). It took $(Inspect.toStr runTime) Milliseconds"

splitLines : Str -> List Str
splitLines = \file ->
    Str.split file "\n"

calculateValuesSum : List Str -> Int Signed32
calculateValuesSum = \calibrationValues ->
    List.map calibrationValues concatenatedStrDigits
    |> List.sum
expect calculateValuesSum ["1abc2", "npqr3stu8vwx", "na1b2c3d4e5f", "treb7uchet"] == 142
expect
    calculateValuesSum [
        "two1nine",
        "eightwothree",
        "abcone2threexyz",
        "xtwone3four",
        "4nineeightseven2",
        "zoneight234",
        "7pqrstsixteen",
    ]
    == 281

concatenatedStrDigits : Str -> I32
concatenatedStrDigits = \processedStr ->
    Str.walkUtf8 processedStr { str: processedStr, acc: [] } \state, byte ->
        utfNumber =
            when Str.fromUtf8 [byte] is
                Ok s -> Str.toI8 s
                _ -> crash "Bad utf"

        startWith = List.findFirst
            baseDigits
            \digit -> Str.startsWith state.str digit
        subString = stringChopFirst state.str
        when (utfNumber, startWith) is
            (Ok number, _) ->
                { str: subString, acc: List.append state.acc number }

            (_, Ok digit) ->
                number = baseDigitsToNumber digit
                { str: subString, acc: List.append state.acc number }

            (_, Err _) ->
                { str: subString, acc: state.acc }

            (Err _, _) ->
                { str: subString, acc: state.acc }
    |> \{ str: _, acc } ->
        acc
    |> \encodedNumbers ->
        List.map encodedNumbers Num.toStr
        |> \stringifiedNumbers ->
            when stringifiedNumbers is
                [first, .., last] -> Str.concat first last
                [first] -> Str.concat first first
                _ -> "0"
    |> \str ->
        when Str.toI32 str is
            Ok num -> num
            Err _ -> 0

expect concatenatedStrDigits "six4294seven8" == 68
expect concatenatedStrDigits "seveneight74five1" == 71
expect concatenatedStrDigits "kfghjdssttgrfour2two5three1nine" == 49
expect concatenatedStrDigits "5gkhgsixvlpcdmpgbjkqtjlthreetwo3one" == 51
expect concatenatedStrDigits "sevenfour1tfxzgsnldk1fiveqg" == 75
expect concatenatedStrDigits "187qhhqlrjx" == 17
expect concatenatedStrDigits "2lthreethreexthjqcsix" == 26
expect concatenatedStrDigits "cgpxkxgkmcpdzhtpc363" == 33
expect concatenatedStrDigits "sixbtkvqt3" == 63
expect concatenatedStrDigits "4twotwoqqmtb7dczgqrtsixseven" == 47
expect concatenatedStrDigits "seventwo7z9fzgjkl5fjndrznc" == 75
expect concatenatedStrDigits "qsgfnjqsghtwoninethree8lppsthree" == 23
expect concatenatedStrDigits "threesevenkdzzvzbl2foursxrlq4sevensix" == 36
expect concatenatedStrDigits "seven8bjbmcnhrcp3one" == 71
expect concatenatedStrDigits "gg531" == 51
expect concatenatedStrDigits "one8sfvzplxlknine54" == 14
expect concatenatedStrDigits "dlxeightwo48fourthreeninethreeeight" == 88
expect concatenatedStrDigits "sixxlglqsjtmqmgcnfvpljsevenqlgllg6" == 66
expect concatenatedStrDigits "tszncfour7one" == 41
expect concatenatedStrDigits "3lkjl1kj8lkj" == 38
expect concatenatedStrDigits "" == 0

# TODO this might be better converted to a record of digit -> number
baseDigits : List Str
baseDigits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
baseDigitsToNumber = \digit ->
    when digit is
        "one" -> 1
        "two" -> 2
        "three" -> 3
        "four" -> 4
        "five" -> 5
        "six" -> 6
        "seven" -> 7
        "eight" -> 8
        "nine" -> 9
        _ -> crash "Should not reach here"

stringChopFirst = \s ->
    Str.toUtf8 s
    |> List.split 1
    |> \{ others } -> others
    |> Str.fromUtf8
    |> Result.withDefault ""
