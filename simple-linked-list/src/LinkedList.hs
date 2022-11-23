module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

-- I know this is cheating but I don't know how to implement `datum` when list
-- is empty?
data LinkedList a = LinkedList [a] deriving (Eq, Show)

datum :: LinkedList a -> a
datum (LinkedList xs) = head xs

fromList :: [a] -> LinkedList a
fromList xs = LinkedList xs

isNil :: LinkedList a -> Bool
isNil (LinkedList xs) = null xs

new :: a -> LinkedList a -> LinkedList a
new x (LinkedList xs) = LinkedList (x : xs)

next :: LinkedList a -> LinkedList a
next (LinkedList xs) = LinkedList (tail xs)

nil :: LinkedList a
nil = LinkedList []

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList (LinkedList xs) = LinkedList (reverse xs)

toList :: LinkedList a -> [a]
toList (LinkedList xs) = xs
