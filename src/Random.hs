module Random where

import System.Random

-- Get a random item out of a list
selectRandomElement :: [a] -> IO a
selectRandomElement [] = error "Cannot select an element from an empty list."
selectRandomElement list = randomIntWithinRange >>=
  \r -> return $ list !! r
  where
  randomIntWithinRange = getStdRandom $ randomR (0, length list - 1)


-- Alternate way of selecting a random elements from a passed in range
getRandomElementsFromRange :: Int -> [a] -> IO [a]
getRandomElementsFromRange _ [] = error "Range is empty"
getRandomElementsFromRange size range = do
  -- Create the random number generator
  g <- newStdGen
  -- Get a list of random integers which will map the random elements to choose
  let randomInts = take size $ randomRs (0, length range - 1) g
  return $ map (\n -> range !! n) randomInts
