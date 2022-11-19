module School (School, add, empty, grade, sorted) where

import Data.Map (Map)
import qualified Data.Map as M

newtype School = School (Map Int [String])

add :: Int -> String -> School -> School
add gradeNum student (School gradeStudents) = School $ M.insert gradeNum newSortedGrade gradeStudents
  where
    newSortedGrade = sortedInsert student $ M.findWithDefault [] gradeNum gradeStudents

sortedInsert :: Ord a => a -> [a] -> [a]
sortedInsert y [] = [y]
sortedInsert y (x : xs) = if y <= x
                          then y : x : xs
                          else x : sortedInsert y xs

empty :: School
empty = School (M.empty)

grade :: Int -> School -> [String]
grade gradeNum (School gradeStudents) = M.findWithDefault [] gradeNum gradeStudents

sorted :: School -> [(Int, [String])]
sorted (School gradeStudents) = M.toAscList gradeStudents
