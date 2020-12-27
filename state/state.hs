data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

coin, push :: TurnstileState -> (TurnstileOutput, TurnstileState)

coin _ = (Thank, Unlocked)

push Locked   = (Tut , Locked)
push Unlocked = (Open, Locked)


monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
  let (a1, s1) = coin s0
      (a2, s2) = push s1
      (a3, s3) = push s2
      (a4, s4) = coin s3
      (a5, s5) = push s4
  in ([a1, a2, a3, a4, a5], s5)

regularPerson :: TurnstileState -> ([TurnstileOutput], TurnstileState)
regularPerson s0 =
    let (a1, s1) = coin s0
        (a2, s2) = push s1
    in ([a1, a2], s2)

distractedPerson :: TurnstileState -> ([TurnstileOutput], TurnstileState)
distractedPerson s0 =
    let (a1, s1) = coin s0
    in ([a1], s1)

hastyPerson :: TurnstileState -> ([TurnstileOutput], TurnstileState)
hastyPerson s0
    | s0 == Unlocked =
        let (a1, s1) = push s0
        in ([a1], s1)
    | otherwise =
        let (a1, s1) = push s0
            (a2, s2) = coin s1
            (a3, s3) = push s2
        in ([a1, a2, a3], s3)


tuesday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
tuesday s0 =
    let (a1, s1) = regularPerson s0
        (a2, s2) = hastyPerson s1
        (a3, s3) = distractedPerson s2
        (a4, s4) = hastyPerson s3
    in (a1 ++ a2 ++ a3 ++ a4, s4)

main = do
    print $ monday Locked
    print $ tuesday Locked
