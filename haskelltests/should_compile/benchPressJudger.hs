benchPressJudger :: (Ord a, Fractional a) => a -> String  
benchPressJudger benchPressLbs 
    | benchPressKgs <= weak = "Stop programming and hit the gym!"  
    | benchPressKgs <= decent = "This is decent, you're doing ok."  
    | benchPressLbs <= strong    = "Nice, you're getting pretty strong."  
    | otherwise     = "Ok, get off the roids."  
    where benchPressKgs = benchPressLbs * 0.4535
          weak = 67
          decent = 125 
          strong = 250