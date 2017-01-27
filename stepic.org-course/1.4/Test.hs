module Test where

dist a b =
    let dx = fst a - fst b
        dy = snd a - snd b
     in sqrt $ dx * dx + dy * dy
