module ValentinesDay where

-- Define the function and required algebraic data types (ADT) below.

data Approval = Yes | No | Maybe

data Cuisine = Korean | Turkish

data Genre = Crime | Horror | Romance | Thriller

data Activity = BoardGame | Chill | Movie Genre | Restaurant Cuisine | Walk Int

-- This looks more gross than just case splitting on `activity`, right?
rateActivity :: Activity -> Approval
rateActivity BoardGame = No
rateActivity Chill = No
rateActivity (Movie genre) =
    case genre of
      Romance -> Yes
      _ -> No
rateActivity (Restaurant cuisine) =
    case cuisine of
      Korean -> Yes
      Turkish -> Maybe
rateActivity (Walk km)
  | km < 3 = Yes
  | 3 <= km && km <= 5 = Maybe
  | otherwise = No
