class (Show a) => ShowFrench a where
    showFrench :: a -> String

instance ShowFrench Int where
    showFrench 0 = "zéro"
    showFrench 1 = "un"
    showFrench 2 = "deux"
    showFrench 3 = "trois"
    showFrench x = show x


instance ShowFrench Bool where
    showFrench True = "vrai"
    showFrench False = "faux"