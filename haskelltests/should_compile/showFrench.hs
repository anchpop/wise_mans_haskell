-- We'll only need one function in our typeclass - showFrench, which takes a value and returns a String
class ShowFrench a where
    showFrench :: a -> String

-- Now, let's make Int a member of this typeclass
instance ShowFrench Int where
    showFrench 0 = "zéro"
    showFrench 1 = "un"
    showFrench 2 = "deux"
    showFrench 3 = "trois"
    -- this pattern match isn't complete, but that's ok, this is just for demonstration purposes

-- Let's also make Boolean an instance
instance ShowFrench Bool where
    showFrench True = "vrai"
    showFrench False = "faux"