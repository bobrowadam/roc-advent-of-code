module [
    u8ToUtfNumber,
]

import Option exposing [Option]

u8ToUtfNumber : U8 -> Option U8
u8ToUtfNumber = \u8Number ->
    when u8Number is
        48 -> Some 0
        49 -> Some 1
        50 -> Some 2
        51 -> Some 3
        52 -> Some 4
        53 -> Some 5
        54 -> Some 6
        55 -> Some 7
        56 -> Some 8
        57 -> Some 9
        _ -> None
