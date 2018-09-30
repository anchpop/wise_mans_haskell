energy :: (Num a) => a -> a
energy mass = 
    let cSquared = 299792458^2
    in mass * cSquared