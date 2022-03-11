-- Name: Kelvin Lu


module Homework4 where

import Prelude hiding (zipWith,any)

-- @Author Mark Snyder
-- Example is from class.  
qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = let pivot   = x
                   lowers  = filter (\n -> n <= pivot) xs
                   highers = filter (\n -> n >  pivot) xs
               in (qsort lowers) ++ [pivot] ++ (qsort highers)
--------------------------------------------------------------------------------
-- Defines the first two integers of the fib sequence.
-- Recursively calls fibhelper to iteratively add for fib. 
-- Parameters: n - Number of iteration of fib. 
-- Return: Fib value at nth iteration. 
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fibHelper (0,1) (n-1)

-- Recursively loops until n-iteration is reached. 
-- Adds the last two calculated value of fib and call fibhelper.
-- Parameter: (a,b)- Tuple of (firstNum, secondNum) to be added; n - Current iteration
-- Return: The fib value at n iteration. 
fibHelper:: (Int, Int) -> Int -> Int
fibHelper (a, b) 1 = a+b
fibHelper (a, b) n = fibHelper (b, (a+b)) (n-1)


--------------------------------------------------------------------------------
-- Iterates until it reaches to last element of list.
-- Concatenates elements starting from tail to start.
-- Parameters: (x:xs) - The list ot be reversed. 
-- Return: The reversed Linked list. 
reversed :: [a] -> [a]
reversed [] = []
reversed (x:xs) = (reversed xs) ++ [x]
--------------------------------------------------------------------------------
-- If the number is less than 2, return false
-- Otherwise call on the isPrimeHelper that starts at range 2
-- Parameter: Number to be processed. 
-- Return: Boolean value, true if n is prime, false otherwise. 
isPrime :: Int -> Bool
isPrime n | n<2       = False
          | otherwise = isPrimeHelper n 2

-- Use variable x to iteriatively check for possible divisors.
-- TODO: Implement sqrt for n to reduce iterations. 
isPrimeHelper:: Int -> Int -> Bool
isPrimeHelper n x | x >= n  = True
                  | (mod n x == 0) = False
                  | otherwise      = isPrimeHelper n (x+1)

--------------------------------------------------------------------------------
-- Iterate every elements of the list.
-- Cons only the element that is not duplicated. 
-- Uses Haskell's filter function to remove duplicate elements. 
-- Parameter: (x:xs) - The list to be processed. 
-- Return: A non-duplicate list. 
nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs) = x : filter (/= x) (nub xs)

--------------------------------------------------------------------------------
-- Perform function f on each element and concatenates it.
-- Recursively right associative call function until one of the lists run out elements. 
-- Parameters:  f - Operator to apply on; (x:xs) - The first list; (y:ys)- The second list
-- Return:  A list, where an operation is done on each element of the 2 parameters. 
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _  [] _ = [] -- Empty b
zipWith _  _ [] = [] -- Empty a
zipWith f (x:xs) (y:ys) = (f x y) : zipWith f xs ys
--------------------------------------------------------------------------------
-- If n is equalt to 1, append, 
-- Else if n is even, concat collatz (div n 2)
-- Else if n is odd, concat collatz (3*n+1)
-- Parameter:  Input Number
-- Return:  The resulting list with each instance of calculation.
collatz :: Int -> [Int]
collatz n | n == 1 = [1]
          | even n = [n] ++ collatz (div n 2) -- Use of integer division for type casting.
          | odd n =  [n] ++ collatz (n*3+1)
collatz _ = [1]

--------------------------------------------------------------------------------

-- Place a sorted list and length of the list.
-- Parameter: xs - List to be processed.
-- Return: The median of the list. 
median :: [Int] -> Double
median [] = error "Empty List"
median xs = medianHelper (qsort xs) (length xs)

-- Caculates the averages of two values. 
avg x y= (x+y)/2.0

-- Calculates the middle index of list. 
midIndex n = div n 2

-- If len of list is odd, return middle index element
-- Otherwise - return the average of the two middle elements. 
-- Parameter: xs-List to be processed; len - length of the list
-- Return: The median of the list. 
medianHelper :: [Int] -> Int -> Double
medianHelper [] _ = error "Empty List"
medianHelper [x] _ = (fromIntegral x :: Double)
medianHelper xs len = if odd len
                          then ((fromIntegral (xs !! (midIndex len))) :: Double) -- Return middle index
                          else avg (fromIntegral (xs !! ((midIndex len )-1)) :: Double)  (fromIntegral (xs !! (midIndex len)) :: Double) 


--------------------------------------------------------------------------------



-- Returns the list of keys with max occurence in list.
-- Calls on modeHelper to create the list.  
mode :: [Int] -> [Int] --Int 
mode [] = error "List is empty"
mode (x:xs) = modeHelper 0 (nub(x:xs)) (x:xs) []

-- Returns a dictionary with tuple that contains the maxOccurence and its respective keys. 
-- Generates a list of keys with maxOccurence inside list. 
-- Use comparison to iterate through list at O(n) complexiity. 
-- Parameter: maxOccur - Current Max Occurence; keys - (Unique keys being compared); xs - Original List; output - Output for function. 
-- Return: A list of keys with max occurences. 
modeHelper:: Int -> [Int] -> [Int] -> [Int]-> [Int]
modeHelper  _    []    []   []    = error "Empty list"
modeHelper  _    []     _   output     = output -- All keys compared. 
-- modeHelper  _    _     []   _     = []
modeHelper maxOccur (k:keys) xs output
  | iterateMode k xs > maxOccur = modeHelper (iterateMode k xs) keys xs [k] -- Creates a new list with key. 
  | iterateMode k xs == maxOccur = modeHelper maxOccur keys xs (output ++ [k]) -- Append output with the key if same number of occurence.
  | otherwise = modeHelper maxOccur keys xs output -- Skip current instance if key is not the mode.

-- Return the number occurence of a key inside a list. 
iterateMode :: Int -> [Int] -> Int
iterateMode _ [] = 0
iterateMode key xs = length (filter (== key) xs)

--------------------------------------------------------------------------------

-- Assumption: Each value is a single digit, and it's a 9x9 sodoku block. 
-- Parameter: 2D List to be processed. 
-- Return - Boolean if the sudoku solution is valid. 
-- TODO- Check Block is not implemented. 
checkSudoku :: [[Int]] -> Bool
checkSudoku xs = checkRow xs && (checkCol 0 xs) -- && (checkBlock xs)

-- Checks each rows, making sure each unique value adds up to 45. 
checkRow :: [[Int]] -> Bool
checkRow [] = True 
checkRow (row: xs) = if (((sum (nub row)) == 45) && (length (nub row) == 9)) 
                     then checkRow xs 
                    else False 

-- Check each columns, making sure each unqique value adds up to 45 and length of column is 9. 
-- If all 9 columns checked, return true, otherwise, check the next col. 
-- Return false if one of the column doesn't meet requirements. 
checkCol :: Int -> [[Int]] -> Bool
checkCol _ [] = True 
checkCol index xs = if (((sum (nub (extractCol index xs))) ==45) && (length (nub (extractCol index xs)) == 9)) 
                     then (if ((index +1) < 9) then checkCol (index + 1) xs else True)
                     else False

-- Extract a col based on column index. 
extractCol :: Int -> [[Int]] -> [Int]
extractCol _         []   = [] 
extractCol index (row:xs) = (row !! index) : (extractCol index xs)

-- Check each columns, making sure each unqique value adds up to 45. 
checkBlock :: [[Int]] -> Bool
checkBlock [] = True 
-- checkBlock xs = (check3Blocks 0 (map (xs !!) [0,1,2])) && (check3Blocks 0 (map (xs !!) [3,4,5])) && (check3Blocks 0 (map (xs !!) [6,7,8]))

-- checkBlocks xs = (checkBlock (map (xs !!) [0,1,2]) extractCol [0,1,2]

-- checkBlocks :: Int -> Bool
-- checkBlocks row col xs 
-- Checks three 3x3 blocks in a list of 3 rows. 
-- If n==3, then all 3 blocks are checked. 
-- Second statement- Check if the condition of current block
--                       If condition met, check next block. 
-- Otherwise false if second conditoin isn't met.
-- Parameters: xs - An input of 3 rows
-- Return: A boolean to check if the 3 blocks are good. 
-- check3Blocks:: Int -> [[Int]] -> Bool
-- check3Blocks _ [] = error "Empty list"
-- check3Blocks n xs | (n == 3) = True -- All 3 blocks are checked. 
--                   | ((sum (nub (formBlock xs))) == 45) && (length (nub (formBlock ( xs))))==9 = check3Blocks (n + 1) (drop 1 xs) 
--                   | otherwise = False

-- formBlock :: [[Int]] -> [Int]
-- formBlock [] = [] 
-- formBlock (r:rows) = [map (r !!) [0,1,2]] ++ (formBlock (rows))
--------------------------------------------------------------------------------
