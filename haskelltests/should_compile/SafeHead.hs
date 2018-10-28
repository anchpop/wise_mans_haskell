{-# LANGUAGE GADTs #-}

data Empty
data NonEmpty

data List a tag where
        End  :: List a Empty
        Cons :: a -> List a tag -> List a NonEmpty

safeHead :: List a NonEmpty -> a
safeHead (Cons a b) = a