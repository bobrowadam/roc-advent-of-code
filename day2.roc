app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.12.0/Lb8EgiejTUzbggO2HVVuPJFkwvvsfW6LojkLR20kTVE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.7.1/MvLlME9RxOBjl0QCxyn3LIaoG9pSlaNxCa-t3BfbPNc.tar.br",
}

import pf.Stdout
import pf.Task
import parser.String exposing [parseStr, digits]
import "./day2-input.txt" as inputFile : Str

main =
    fileLines = List.keepIf (Str.split inputFile "\n") \line -> Bool.isNotEq line ""
    gamesPlayed = List.map fileLines \lines -> parsedLine lines
    gamesPowers = List.map gamesPlayed gamePower
    sumOfGamePowers = List.sum gamesPowers
    Stdout.line! "Sum of game indexes $(Inspect.toStr sumOfGamePowers)"

gamePower = \game ->
    maxOfEachColorInGame game
    |> \{ red, blue, green } -> red * blue * green

maxOfEachColorInGame = \game ->
    List.walk game.reveals { red: 0, blue: 0, green: 0 } \state, reveal -> {
        red: Num.max state.red reveal.red,
        blue: Num.max state.blue reveal.blue,
        green: Num.max state.green reveal.green,
    }

testGame = {
    game: 1,
    reveals: [
        { red: 12, green: 13, blue: 14 },
        { red: 18, green: 11, blue: 19 },
        { red: 11, green: 6, blue: 32 },
    ],
}
expect maxOfEachColorInGame testGame == { red: 18, green: 13, blue: 32 }

expect
    power = gamePower testGame
    power == 18 * 13 * 32

testGames = [
    {
        game: 1,
        reveals: [
            { red: 12, green: 13, blue: 14 },
            { red: 18, green: 11, blue: 19 },
            { red: 11, green: 6, blue: 12 },
        ],
    },
    {
        game: 2,
        reveals: [
            { red: 12, green: 13, blue: 14 },
            { red: 8, green: 1, blue: 3 },
            { red: 1, green: 6, blue: 12 },
        ],
    },
    {
        game: 3,
        reveals: [
            { red: 12, green: 13, blue: 14 },
            { red: 1, green: 11, blue: 19 },
            { red: 11, green: 6, blue: 12 },
        ],
    },
]

expect
    sumOfPowers = List.map testGames gamePower |> List.sum
    sumOfPowers == 18 * 13 * 19 + 12 * 13 * 14 + 12 * 13 * 19

parsedLine = \strLine ->
    gameAndReveals =
        Str.splitFirst strLine ":"
        |> Result.map \{ before, after } ->
            revealsStr = Str.split after ";"
            (parseIndex before, List.mapTry revealsStr parseReveal)
        |> Result.map \(indexResult, revealsResult) ->
            when (indexResult, revealsResult) is
                (Ok i, Ok rs) -> (i, rs)
                (Err e, Ok _) -> crash "Error parsing game index $(Inspect.toStr e)"
                (Ok _, Err e) -> crash "Error parsing reaveals  $(Inspect.toStr e)"
                (Err indexError, Err revealsError) -> crash "Error parsing reaveals and index: indexError:  $(Inspect.toStr indexError), reveal error: $(Inspect.toStr revealsError)"

    when gameAndReveals is
        Ok (game, reveals) -> { game, reveals }
        Err NotFound -> crash "Oh oh"

expect
    parsed = parsedLine "Game 1: 3 blue, 4 red, 1 green; 1 red, 2 green, 6 blue"
    parsed.reveals == [{ blue: 3, red: 4, green: 1 }, { red: 1, green: 2, blue: 6 }]

parseIndex = \indexStr ->
    Str.splitLast indexStr " "
    |> Result.map \{ after } -> after
    |> Result.try Str.toI32

parseReveal = \revealStr ->
    Str.split revealStr ","
    |> List.map Str.trimStart
    |> List.map \s -> Str.splitFirst s " "
        |> Result.try \{ before, after } ->
            Result.try (parseStr digits before) \digit -> Ok (after, digit)
    |> List.walkTry { green: 0, red: 0, blue: 0 } \state, colorCountResult ->
        when colorCountResult is
            Ok colorCount ->
                when colorCount is
                    ("green", n) -> Ok { state & green: n }
                    ("blue", n) -> Ok { state & blue: n }
                    ("red", n) -> Ok { state & red: n }
                    (invalidColor, _) -> Err "color is not allowd: invalidColor: $(invalidColor)"

            Err e -> crash "Error in parseReveal: $(Inspect.toStr e)"

expect
    parsed = parseReveal "3 blue, 4 red, 3 green"
    parsed == Ok { blue: 3, red: 4, green: 3 }

# when parsed is
#     ()

# |> \strList -> Result.map strList \{ before, after } -> before
# |> List.map \{ before, after } -> (parseStr digits before, after)

# gameLineUtf8 = Str.toUtf8 "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

# expect gameTest.blue == 3

# parseTestInput1 =
#     """
#     Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
#     Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
#     Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
#     Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
#     Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
#     """
