module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Robot Bearing (Integer, Integer)

bearing :: Robot -> Bearing
bearing (Robot direction _) = direction

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ coords) = coords

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

move :: Robot -> String -> Robot
move robot "" = robot
move robot ('A' : cs) = move (advance robot) cs
move robot ('L' : cs) = move (left robot) cs
move robot ('R' : cs) = move ((left . left . left) robot) cs
move robot (_ : cs) = move robot cs

advance :: Robot -> Robot
advance (Robot North (x, y)) = Robot North (x, y + 1)
advance (Robot East  (x, y)) = Robot East  (x + 1, y)
advance (Robot South (x, y)) = Robot South (x, y - 1)
advance (Robot West  (x, y)) = Robot West  (x - 1, y)

left :: Robot -> Robot
left (Robot North coords) = Robot West  coords
left (Robot East  coords) = Robot North coords
left (Robot South coords) = Robot East  coords
left (Robot West  coords) = Robot South coords
