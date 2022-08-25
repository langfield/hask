module LuciansLusciousLasagna (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes) where

-- TODO: define the expectedMinutesInOven constant
expectedMinutesInOven :: Integer
expectedMinutesInOven = 40

-- TODO: define the preparationTimeInMinutes function
preparationTimeInMinutes :: Int -> Int
preparationTimeInMinutes numLayers = 2 * numLayers

-- TODO: define the elapsedTimeInMinutes function
elapsedTimeInMinutes :: Int -> Int -> Int
elapsedTimeInMinutes numLayers ovenMinutes = preparationTimeInMinutes(numLayers) + ovenMinutes
