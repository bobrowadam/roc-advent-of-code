module [
    flatMap,
    Option,
    isSome,
    isNone,
    keepSome,
    map,
]

Option a : [Some a, None]

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

keepSome : List (Option a) -> List a
keepSome = \options ->
    List.walk options [] \state, option ->
        when option is
            Some d -> List.concat state [d]
            None -> state

map : Option a, (a -> b) -> Option b
map = \optionA, aTob ->
    when optionA is
        Some a -> Some (aTob a)
        None -> None

flatMap : Option a, (a -> Option b) -> Option b
flatMap = \optionA, aToOptionB ->
    when optionA is
        Some a -> aToOptionB a
        None -> None

expect
    optionA = Some 5
    res = map optionA \a -> a + a
    res == Some 10

expect
    optionA = None
    res = map optionA \a -> a + a
    res == None

expect
    optionA = Some 5
    res = flatMap optionA \a -> Some (a + a)
    res == None
