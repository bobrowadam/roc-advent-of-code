app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.12.0/Lb8EgiejTUzbggO2HVVuPJFkwvvsfW6LojkLR20kTVE.tar.br",
}
import pf.Stdout
import pf.Task
import "./input.txt" as inputFile : Str

main =
    parsedLines = splitLines inputFile
    res = calculateValuesSum parsedLines
    Stdout.line! "finished with: $(Inspect.toStr res)"

splitLines : Str -> List Str
splitLines = \file ->
    Str.split file "\n"

calculateValuesSum : List Str -> (Int Signed32)
calculateValuesSum = \calibrationValues ->
    List.map calibrationValues concatenatedStrDigits
    |> List.sum

concatenatedStrDigits = \str ->
    res = (Str.toUtf8 str)
        |> List.map \utfNum -> Str.fromUtf8 [utfNum]
        |> List.keepOks \d -> d
        |> List.map Str.toU8
        |> List.keepOks \n -> n
        |> List.map Num.toStr
        |> \encodedNumbers ->
            when encodedNumbers is
                [first, .., last] -> Str.concat first last
                _ -> "0"
        |> Str.toI32

    when res is
        Ok num -> num
        Err e ->
            dbg e
            0

expect concatenatedStrDigits "3lkjl1kj8lkj" == 38
expect concatenatedStrDigits "" == 0
