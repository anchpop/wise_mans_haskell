{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

data Nat = Zero | Succ Nat

data List a n where
    End  :: List a 'Zero 
    Cons :: a -> List a n -> List a ('Succ n)

safeHead :: List a ('Succ n) -> a
safeHead (Cons a as) = a

safeTail :: List a ('Succ n) -> List a n
safeTail (Cons a as) = as