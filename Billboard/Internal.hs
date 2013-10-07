module Billboard.Internal where

-- | applies a function to the last element of a list
updateLast :: (a -> a) -> [a] -> [a]
updateLast _ [ ]    = []
updateLast f [x]    = [f x]
updateLast f (x:xs) = x : updateLast f xs
