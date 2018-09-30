data InvitationResponse = Attending | NotAttending | MightAttend

-- We should write a function to check to see if two InvitationResponse values are equal.
-- By "equal" we mean the same. So two InvitationResponse values are equal if they're both Attending, NotAttending, or MightAttend.
-- For that, we'll use pattern matching.
-- We'll also put the name of the function in the middle of the two arguments, for greater readability. This is allowed because when you put ` backticks around a function, it becomes an infix function.
invitationResponseIsEqual :: InvitationResponse -> InvitationResponse -> Bool
Attending `invitationResponseIsEqual` Attending = True        -- If both inputs are Attending, return True.
NotAttending `invitationResponseIsEqual` NotAttending = True  -- If both inputs are NotAttending, return True.
MightAttend `invitationResponseIsEqual` MightAttend = True    -- If both inputs are MightAttend, return True.
_ `invitationResponseIsEqual` _ = False                       -- Otherwise, the two inputs can't be the same, so return False.

-- Yes, you can pattern match like that. Let's make a function that turns 
-- an InvitationResponse and a name into a message to the person who made the invitation.

makeMessage :: String -> InvitationResponse -> String
makeMessage name Attending = "Yes, " ++ name ++ " can attend your event!"
makeMessage name NotAttending = "Sorry, " ++ name ++ " can't attend your event."
makeMessage name MightAttend = name ++ " might attend your event."