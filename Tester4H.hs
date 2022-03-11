{-

AUTHOR - MARK SNYDER
load this file and call main. Alternatively, run this command to run
just the expression "main", from the terminal, without going interactive:

      

All errors and failures count against your score out of 100 points.

You can also use the testFunc function to only test a specific
function (look through the file for the exact name, but it's always
"test_" and the official name from the spec). Example:

    testFunc test_fib

You can also narrow down to a single test case via justTest, e.g. just the 3-indexed test of fib:

    testOne test_fib 3

Be sure to :reload between calls!

-}


import Homework4
import Test.HUnit
import Control.Exception
import Control.Monad

import Prelude hiding (zipWith)


-- glue all test lists together to run them all.
main = runTestTT $ TestList [test_fib, test_reversed, test_isPrime, test_nub, test_zipWith, test_collatz, test_median, test_mode, test_checkSudoku]

-- helper to test a single function. (Just a friendlier name)
testFunc tl = runTestTT tl

-- helper to test a single test of a single function (builds a
-- TestList with the singleton list of just the indexed position
-- requested).
justTest (TestList xs) n = TestList [xs!!n]
-- call this one.
testOne (TestList xs) n = runTestTT $ justTest (TestList xs) n


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- each item in the list is a test case (tc), given some string
-- description, with the expected answer, and then the expression to
-- be comparing.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

test_fib = TestList [
  tc "fib #0: 0"   0 $ fib 0,
  tc "fib #1: 1"   1 $ fib 1,
  tc "fib #2: 2"   1 $ fib 2,
  tc "fib #3: 3"   2 $ fib 3,
  tc "fib #4: 4"   3 $ fib 4,
  tc "fib #5: 5"   5 $ fib 5,
  tc "fib #6: 6"   8 $ fib 6,
  tc "fib #7: 7"  13 $ fib 7,
  tc "fib #8: 20" 6765 $ fib 20,
  tc "fib #9: 29" 514229 $ fib 29,
  tc "fib #10: 43" 433494437 $ fib 43,
  tc "fib #11: 55" 139583862445 $ fib 55]


test_reversed = TestList [
  tc "reversed #0: []" [] $ reversed ([]::[Int]), -- needed to choose a type for tc to work upon.
  tc "reversed #1: [5]" [5] $ reversed [5],
  tc "reversed #2: [10,5]" [5,10] $ reversed [10,5],
  tc "reversed #3: [2,4,6,8]" [8,6,4,2] $ reversed [2,4,6,8],
  tc "reversed #4: [1,1,1,2,1,1]" [1,1,2,1,1,1] $ reversed [1,1,1,2,1,1],
  tc "reversed #5: [1,3,5,2,4,6]" [6,4,2,5,3,1] $ reversed [1,3,5,2,4,6],
  tc "reversed #6: [5,5,5,5,5]"  [5,5,5,5,5] $ reversed [5,5,5,5,5],
  tc "reversed #7: [True, False, False]" [False, False, True] $ reversed [True, False, False],
  tc "reversed #8: \"abcd\"" "dcba" $ reversed "abcd",
  tc "reversed #9: [1..1000]" [1000,999..1] $ reversed [1..1000],
  tc "reversed #10: [[1,2,3],[4,5],[6,7,8]]" [[6,7,8],[4,5],[1,2,3]] $ reversed [[1,2,3],[4,5],[6,7,8]],
  tc "reversed #11: \"lolol\"" "lolol" $ reversed "lolol"
  ]
  
  -- isPrime
test_isPrime = TestList [
  tc "isPrime #0:  (-5)"   False $ isPrime   (-5),
  tc "isPrime #1:  0"      False $ isPrime      0,
  tc "isPrime #2:  1"      False $ isPrime      1,
  tc "isPrime #3:  2"       True $ isPrime      2,
  tc "isPrime #4:  3"       True $ isPrime      3,
  tc "isPrime #5:  4"      False $ isPrime      4,
  tc "isPrime #6:  5"       True $ isPrime      5,
  tc "isPrime #7:  39"     False $ isPrime     39,
  tc "isPrime #8:  41"      True $ isPrime     41,
  tc "isPrime #9:  117"    False $ isPrime    117,
  tc "isPrime #10: 1117"    True $ isPrime   1117,
  tc "isPrime #11: 11117"   True $ isPrime  11117,
  tc "isPrime #12: 111117" False $ isPrime 111117
  ]
  
  
  -- nub
test_nub = TestList [
  tc "nub #0: []"                                              []  $ nub ([]::[Int]), -- we have to choose a type in the list to make the tester happy.
  tc "nub #1: [5]"                                            [5]  $ nub [5],
  tc "nub #2: [13,13,13]"                                    [13]  $ nub [13,13,13],
  tc "nub #3: [1,2,3,1,2,3,1,2,3]"                        [1,2,3]  $ nub [1,2,3,1,2,3,1,2,3],
  tc "nub #4: [1,1,3,2,2,5,5,5,5,4]"                  [1,3,2,5,4]  $ nub [1,1,3,2,2,5,5,5,5,4],
  
  tc "nub #5: [1,4,2,5,3,1,2,3,4,5,6,1,3,2]"        [1,4,2,5,3,6]  $ nub [1,4,2,5,3,1,2,3,4,5,6,1,3,2],
  tc "nub #6: [1,2,3,4,5]"                            [1,2,3,4,5]  $ nub [1,2,3,4,5],
  tc "nub #7: [-3,-1,1,3,2,4,-2,-4]"        [-3,-1,1,3,2,4,-2,-4]  $ nub [-3,-1,1,3,2,4,-2,-4],
  tc "nub #8: [1,1,5,1,10,1,1,15,1,1]"                [1,5,10,15]  $ nub [1,1,5,1,10,1,1,15,1,1],
  tc "nub #9: [1,2,3,2]"                                  [1,2,3]  $ nub [1,2,3,2],
  
  tc "nub #10: [1,2,3,3]"                                  [1,2,3] $ nub [1,2,3,3],
  tc "nub #11: \"lolol\""                                     "lo" $ nub "lolol"
  ]


test_zipWith = TestList [
  tc "zipWith #0:   (+) [1,2,3,4] [10,10,10,10]" [11,12,13,14]$ zipWith (+) [1,2,3,4] [10,10,10,10],
  tc "zipWith #1:   (+) [1,2,3,4] [5,6,7,8]" [6,8,10,12]$ zipWith (+) [1,2,3,4] [5,6,7,8],
  tc "zipWith #2:   (*) [2,3,4] [5,5,5,5,5]" [10,15,20] $ zipWith (*) [2,3,4] [5,5,5,5],
  tc "zipWith #3:   (*) [2,3,4,5,6,7,8] [5,5,5]" [10,15,20] $ zipWith (*) [2,3,4,5,6,7,8] [5,5,5],
  tc "zipWith #4:   (*) [1,2,3,4,5] []" [] $ zipWith (*) [1,2,3,4,5] ([]::[Int]),
  tc "zipWith #5:   (*) [] [1,2,3,4,5]" [] $ zipWith (*) ([]::[Int]) [1,2,3,4,5],
  tc "zipWith #6:   (*) [] []" [] $ zipWith (*) ([]::[Int]) ([]::[Int]),
  tc "zipWith #7:   max [1,2,3,4,5] [5,4,3,2,1]" [5,4,3,4,5] $ zipWith max [1,2,3,4,5] [5,4,3,2,1],
  tc "zipWith #8:   (^) [2,2,2] [4,5,6]" [16,32,64] $ zipWith (^) [2,2,2] [4,5,6],
  tc "zipWith #9:   (\\x y-> if even x then x else y) [1,2,3,4,5] [6,7,8,9,10]" [6,2,8,4,10] $ zipWith (\x y-> if even x then x else y) [1,2,3,4,5] [6,7,8,9,10],
  tc "zipWith #10:  (++) [\"a\",\"b\",\"c\"] [\"x\",\"y\",\"z\"]" ["ax","by","cz"] $ zipWith (++) ["a","b","c"] ["x","y","z"],
  tc "zipWith #11:  (:) [1,2,3] [[10],[20,20],[30,30,30]]" [[1,10],[2,20,20],[3,30,30,30]] $ zipWith (:) [1,2,3] [[10],[20,20],[30,30,30]],
  tc "zipWith #12:  (&&) [False, False, True, True] [False, True, False, True]" [False, False, False, True] $ zipWith (&&) [False, False, True, True] [False, True, False, True],
  tc "zipWith #13:  (||) [False, False, True, True, False] [False, True, False, True]" [False, True, True, True] $ zipWith (||) [False, False, True, True, False] [False, True, False, True]
  ]


test_collatz = TestList [
  tc "collatz #0: 1"    [1]  $ collatz    1,
  tc "collatz #1: 2"    [2, 1]  $ collatz    2,
  tc "collatz #2: 3"    [3, 10, 5, 16, 8, 4, 2, 1] $ collatz    3,
  tc "collatz #3: 4"    [4, 2, 1] $ collatz    4,
  tc "collatz #4: 5"    [5, 16, 8, 4, 2, 1] $ collatz    5,
  tc "collatz #5: 6"    [6,3,10,5,16,8,4,2,1]  $ collatz    6,
  tc "collatz #6: 10"   [10, 5, 16, 8, 4, 2, 1] $ collatz   10,
  tc "collatz #7: 11"   [11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1] $ collatz   11,
  tc "collatz #8: 17"   [17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1] $ collatz   17,
  tc "collatz #9: 42"   [42, 21, 64, 32, 16, 8, 4, 2, 1] $ collatz   42,
  tc "collatz #10: 99"   [99, 298, 149, 448, 224, 112, 56, 28, 14, 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1] $ collatz   99,
  tc "collatz #11: 100"  [100, 50, 25, 76, 38, 19, 58, 29, 88, 44, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1] $ collatz  100,
  tc "collatz #12: 8192" [8192, 4096, 2048, 1024, 512, 256, 128, 64, 32, 16, 8, 4, 2, 1] $ collatz 8192
  ]

test_median = TestList [
  tc "median #0: [1,1,1]"     1.0 $ median [1,1,1],
  tc "median #1: [1,2,3]"     2.0 $ median [1,2,3],
  tc "median #2: [1,2,2,3]"   2.0 $ median [1,2,2,3],
  tc "median #3: [1,2,3,4]"   2.5 $ median [1,2,3,4],
  tc "median #4: [1,3,2,3,1]" 2.0 $ median [1,3,2,3,1],
  tc "median #5: [5,5,5,25]"  5.0 $ median [5,5,5,25],
  tc "median #6: [13,6,13,3,7,29,12,1,2,14]" 9.5 $ median [13,6,13,3,7,29,12,1,2,14]
  ]

  
test_mode = TestList [
  tc "mode #0: [1,1,1]"      [1]       $ mode [1,1,1],
  tc "mode #1: [1,2,3]"      [1,2,3]   $ mode [1,2,3],
  tc "mode #2: [1,2,2,3]"    [2]       $ mode [1,2,2,3],
  tc "mode #3: [1,2,3,4]"    [1,2,3,4] $ mode [1,2,3,4],
  tc "mode #4: [1,3,2,3,1]"  [1,3]     $ mode [1,3,2,3,1],
  tc "mode #5: [5,5,5,25]"   [5]       $ mode [5,5,5,25],
  tc "mode #6: [13,6,13,3,7,29,12,1,2,14]" [13] $ mode [13,6,13,3,7,29,12,1,2,14]
  ]

  
test_checkSudoku = TestList [
  tc "checkSudoku #0: grid_good1"     True $ checkSudoku grid_good1,
  tc "checkSudoku #1: grid_rows_bad" False $ checkSudoku grid_rows_bad,
  tc "checkSudoku #2: grid_cols_bad" False $ checkSudoku grid_cols_bad,
  tc "checkSudoku #3: grid_grps_bad" False $ checkSudoku grid_grps_bad,
  tc "checkSudoku #4: grid_bad_5"    False $ checkSudoku grid_bad_5,
  tc "checkSudoku #5: grid_bad_ones" False $ checkSudoku grid_bad_ones,
  tc "checkSudoku #6: grid_good2"     True $ checkSudoku grid_good2,
  tc "checkSudoku #7: grid_good3"     True $ checkSudoku grid_good3,
  tc "checkSudoku #8: grid_good2"     True $ checkSudoku grid_good2, -- repeated for more credit
  tc "checkSudoku #9: grid_good3"     True $ checkSudoku grid_good3  -- repeated for more credit
 ]

tc s a b = TestCase $ assertEqual s a b

-- from http://stackoverflow.com/questions/6147435/is-there-an-assertexception-in-any-of-the-haskell-test-frameworks
assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
    handleJust isWanted (const $ return ()) $ do
        action
        assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex)

grid_good1  = [[1,2,3, 4,5,6, 7,8,9],
               [4,5,6, 7,8,9, 1,2,3],
               [7,8,9, 1,2,3, 4,5,6],
               
               [2,3,4, 5,6,7, 8,9,1],
               [5,6,7, 8,9,1, 2,3,4],
               [8,9,1, 2,3,4, 5,6,7],
               
               [3,4,5, 6,7,8, 9,1,2],
               [6,7,8, 9,1,2, 3,4,5],
               [9,1,2, 3,4,5, 6,7,8]
              ]

grid_rows_bad = [[1,2,3, 1,2,3, 1,2,3],
                 [4,5,6, 4,5,6, 4,5,6],
                 [7,8,9, 7,8,9, 7,8,9],
                 [2,3,4, 2,3,4, 2,3,4],
         [5,6,7, 5,6,7, 5,6,7],
         [8,9,1, 8,9,1, 8,9,1],
                 
         [3,4,5, 3,4,5, 3,4,5],
         [6,7,8, 6,7,8, 6,7,8],
         [9,1,2, 9,1,2, 9,1,2]
                 ]

grid_cols_bad = [[1,2,3, 4,5,6, 7,8,9],
         [4,5,6, 7,8,9, 1,2,3],
         [7,8,9, 1,2,3, 4,5,6],
                 
         [1,2,3, 4,5,6, 7,8,9],
         [4,5,6, 7,8,9, 1,2,3],
         [7,8,9, 1,2,3, 4,5,6],
                 
         [1,2,3, 4,5,6, 7,8,9],
         [4,5,6, 7,8,9, 1,2,3],
         [7,8,9, 1,2,3, 4,5,6]
                 ]

grid_grps_bad = [[1,2,3, 4,5,6, 7,8,9],
         [2,3,4, 5,6,7, 8,9,1],
         [3,4,5, 6,7,8, 9,1,2],
         [4,5,6, 7,8,9, 1,2,3],
         [5,6,7, 8,9,1, 2,3,4],
         [6,7,8, 9,1,2, 3,4,5],
         [7,8,9, 1,2,3, 4,5,6],
         [8,9,1, 2,3,4, 5,6,7],
         [9,1,2, 3,4,5, 6,7,8]
                 ]

grid_bad_5 = [[1,2,3, 4,5,6, 7,8,9],
              [4,5,6, 7,8,9, 1,2,3],
              [7,8,9, 1,2,3, 4,5,6],
              
              [2,3,4, 5,6,7, 8,9,1],
              [5,6,7, 8,5,1, 2,3,4],
              [8,9,1, 2,3,4, 5,6,7],
              
              [3,4,5, 6,7,8, 9,1,2],
              [6,7,8, 9,1,2, 3,4,5],
              [9,1,2, 3,4,5, 6,7,8]
              ]

grid_bad_ones = [[1,1,1, 1,1,1, 1,1,1],
                 [1,1,1, 1,1,1, 1,1,1],
                 [1,1,1, 1,1,1, 1,1,1],
                 
                 [1,1,1, 1,2,3, 1,1,1],
                 [1,1,1, 4,5,6, 1,1,1],
                 [1,1,1, 7,8,9, 1,1,1],
                 
                 [1,1,1, 1,1,1, 1,1,1],
                 [1,1,1, 1,1,1, 1,1,1],
                 [1,1,1, 1,1,1, 1,1,1]
                 ]

grid_good2 = [[2,7,8, 4,6,9, 1,5,3],
              [6,9,3, 1,2,5, 4,7,8],
              [4,5,1, 7,8,3, 2,6,9],
              
              [7,6,5, 8,9,4, 3,2,1],
              [3,4,2, 6,1,7, 9,8,5],
              [8,1,9, 3,5,2, 6,4,7],
              
              [9,8,4, 5,3,6, 7,1,2],
              [5,2,6, 9,7,1, 8,3,4],
              [1,3,7, 2,4,8, 5,9,6] 
              ]

grid_good3 =[[7,8,6, 4,9,3, 2,5,1],
             [3,2,1, 8,7,5, 4,9,6],
             [9,4,5, 6,2,1, 8,7,3],
             
             [1,5,7, 9,3,8, 6,2,4],
             [2,9,8, 5,4,6, 3,1,7],
             [4,6,3, 7,1,2, 9,8,5],
             
             [8,1,2, 3,6,7, 5,4,9],
             [6,7,4, 2,5,9, 1,3,8],
             [5,3,9, 1,8,4, 7,6,2]
            ]
