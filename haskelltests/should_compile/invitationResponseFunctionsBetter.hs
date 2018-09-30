data InvitationResponse = Attending | NotAttending | MightAttend

instance Eq InvitationResponse where
    Attending == Attending = True        -- If both inputs are Attending, return True.
    NotAttending == NotAttending = True  -- If both inputs are NotAttending, return True.
    MightAttend == MightAttend = True    -- If both inputs are MightAttend, return True.
    _ == _ = False                       -- Otherwise, the two inputs can't be the same, so return False.

-- functions made only of special characters are called operators, and they're  are infix functions by default. That means so you don't have to put backticks around them, and can put them in the middle for definitions.