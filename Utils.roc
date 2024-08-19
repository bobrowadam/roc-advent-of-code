module [
    u8ToUtfNumber,
    isSome,
    isNone,
    keepSomes
]

Option a : [Some a, None]

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

isSome : Option a -> Bool
isSome = \maybeValue ->
    when maybeValue is
        Some _ -> Bool.true
        _ -> Bool.false

isNone : Option a -> Bool
isNone = \maybeValue ->
    when maybeValue is
        None -> Bool.true
        _ -> Bool.false

keepSomes : List (Option a) -> (List a)
keepSomes = \options ->
    List.walk options [] \state, option ->
        when option is
            Some d -> List.concat state [d]
            None -> state
