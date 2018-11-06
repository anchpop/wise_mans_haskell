module Main where
import Text.Read (readMaybe)

data Message = Speaking {texts :: [String], speaker :: String}  | Info {texts :: [String]}  

data Choice = Choice {choice :: String, result :: Adventure}

data Adventure = StoryEnd | StoryMessage Message Adventure | StoryChoice [Choice]

getChoice :: (Integral a, Read a, Show a) => a -> IO a   -- we need to call show on the input, so a needs to be a member of the show typeclass  
getChoice numberOfOptions = do
    putStr "Choice: "
    userChoiceIndex <- getLine
    maybeIndex <- pure . readMaybeInt $ userChoiceIndex 
    case maybeIndex of Nothing -> displayErrorAndRetry               -- note the change
                       Just index -> if index <= numberOfOptions && index > 0
                                       then pure index
                                       else displayErrorAndRetry     -- note the change
  where
    readMaybeInt :: (Integral a, Read a) => String -> Maybe a
    readMaybeInt = readMaybe
    displayErrorAndRetry = do             -- define a function that prints an error then returns getChoice numberOfOptions 
      putStrLn $ "Please enter a number 1 through " ++ (show numberOfOptions) ++ "."
      getChoice numberOfOptions


tellAdventure :: Adventure -> IO ()
tellAdventure StoryEnd = 
  putStrLn "Story over!"
  
tellAdventure (StoryMessage message adventure) = do
  sequence . map putStrLn . texts $ message
  tellAdventure adventure

tellAdventure (StoryChoice choices) = do
  sequence . map putStrLn . map choiceToString $ indexedChoices
  userChoiceIndex <- getChoice (length choices)
  tellAdventure . result . (choices !!) . (\x -> x-1) $ userChoiceIndex
  where indexedChoices = zip [1..] choices
        choiceToString (i, c) = "  " ++ (show i) ++ ": " ++ (choice c)
  

main :: IO ()
main = do
  tellAdventure story
  where story = StoryMessage (Info ["You awaken in a dark cavern, only the dimmest light illuminating your surroundings.", 
                                    "You try to remember how you got there, but you realize you can't remember anything - not even your own name.",
                                    "You look around, and see two pinpricks of light in the distance, one to your left and one to your right.",
                                    "Could they be exits?"]) 
                             (StoryChoice [Choice "Probably not, better to sit here and wait for someone to find me." StoryEnd, 
                                           Choice "Hmm... I'll go explore the possible exit on my right." StoryEnd, 
                                           Choice "I have a good feeling about the light to my left" StoryEnd]) 