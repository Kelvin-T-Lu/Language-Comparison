# Based on testing harness dated 2017-06-02.

# STUDENTS: TO USE:
# 
# The following command will test all test cases on your file:
#   
#   MAC:
#   python3 <thisfile.py> <your_one_file.py>
# 
#   PC:
#   python <thisfile.py> <your_one_file.py>
# 
# 
# You can also limit the tester to only the functions you want tested.
# Just add as many functions as you want tested on to the command line at the end.
# Example: to only run tests associated with func1 and func2, run this command:
# 
#   python3 <thisfile.py> <your_one_file.py> func1 func2
# 
# You really don't need to read the file any further, except that when
# a specific test fails, you'll get a line number - and it's certainly
# worth looking at those areas for details on what's being checked. This would
# all be the indented block of code starting with "class AllTests".


# INSTRUCTOR: TO PREPARE:
#  - add test cases to class AllTests. The test case functions' names must
# be precise - to test a function named foobar, the test must be named "test_foobar_#"
# where # may be any digits at the end, such as "test_foobar_13".
# - any extra-credit tests must be named "test_extra_credit_foobar_#"
# 
# - name all required definitions in REQUIRED_DEFNS, and all extra credit functions
#   in EXTRA_CREDIT_DEFNS. Do not include any unofficial helper functions. If you want
#   to make helper definitions to use while testing, those can also be added there for
#   clarity.
# 
# - to run on either a single file or all .py files in a folder (recursively):
#   python3 <thisfile.py> <your_one_file.py>
#   python3 <thisfile.py> <dir_of_files>
#   python3 <thisfile.py> .                    # current directory
# 
# A work in progress by Mark Snyder, Oct. 2015.
#  Edited by Yutao Zhong,   Spring 2016.
#  Edited by Raven Russell, Spring 2017.
#  Edited by Mark Snyder,   June 2017.
#  Edited by Raven Russell, Spring 2018.
#  Edited by Mark Snyder,   Spring 2019. (imp deprecated; now uses importlib)

import unittest
import shutil
import sys
import os
import time

import importlib.util
import traceback
import platform
import random
import io

random.seed(0)

############################################################################
############################################################################
# BEGIN SPECIALIZATION SECTION (the only part you need to modify beyond 
# adding new test cases).

# name all expected definitions; if present, their definition (with correct
# number of arguments) will be used; if not, a decoy complainer function
# will be used, and all tests on that function should fail.

REQUIRED_DEFNS = [
                   "fib"
                  ,"reversed"
                  ,"is_prime"
                  ,"nub"
                  ,"zip_with"
                  ,"collatz"
                  ,"median"
                  ,"mode"
                  ,"check_sudoku"
                 ]

# for method names in classes that will be tested. They have to be here
# so that we don't complain about missing global function definitions.
# Really, any chosen name for test batches can go here regardless of actual
# method names in the code.
SUB_DEFNS = [ ]

# definitions that are used for extra credit
EXTRA_CREDIT_DEFNS = [  ]

# how many points are test cases worth?
weight_required     = 1
weight_extra_credit = 1

# this is auto-calculated based on all possible tests.
total_points_from_tests = -1

# how many seconds to wait between batch-mode gradings? 
# ideally we could enforce python to wait to open or import
# files when the system is ready but we've got a communication
# gap going on.
DELAY_OF_SHAME = 1

# this gets set in main() for windows/mac/linux
python_command = ""

# warnings to display
warnings = []

# get file contents
def get_file_contents(file_name):
    f = open(file_name,"r", newline='\n')
    contents = f.read()
    f.close()
    
    # remove a newline character at end...
    if contents[-1] == '\n':
        contents = contents[:-1]
        
    return contents

# write file
def set_file_contents(file_name, contents):
    f = open(file_name,"w", newline='\n')
    f.write(contents)
    f.close()

# END SPECIALIZATION SECTION

################################################################################
################################################################################
################################################################################

# enter batch mode by giving a directory to work on as the only argument.
BATCH_MODE = len(sys.argv)==2 and (sys.argv[1] in ["."] or os.path.isdir(sys.argv[1]))

# This class contains multiple "unit tests" that each check
# various inputs to specific functions, checking that we get
# the correct behavior (output value) from completing the call.
class AllTests (unittest.TestCase):
        
    ############################################################################
    
    
    
    #---------------------------------------------------------------------
    
    def test_fib_1(self):  self.assertEqual(fib(0),0)
    def test_fib_2(self):  self.assertEqual(fib(1),1)
    def test_fib_3(self):  self.assertEqual(fib(2),1)
    def test_fib_4(self):  self.assertEqual(fib(3),2)
    def test_fib_5(self):  self.assertEqual(fib(4),3)
    def test_fib_6(self):  self.assertEqual(fib(5),5)
    def test_fib_7(self):  self.assertEqual(fib(6),8)
    def test_fib_8(self):  self.assertEqual(fib(7),13)
    def test_fib_9(self):  self.assertEqual(fib(20),6765)
    def test_fib_10(self): self.assertEqual(fib(29),514229)
    def test_fib_11(self): self.assertEqual(fib(43),433494437)
    def test_fib_12(self): self.assertEqual(fib(55),139583862445)
    
    #---------------------------------------------------------------------
    
    def test_reversed_1(self):  self.assertEqual(reversed([]),[])
    def test_reversed_2(self):  self.assertEqual(reversed([5]),[5])
    def test_reversed_3(self):  self.assertEqual(reversed([2,4,6,8]),[8,6,4,2])
    def test_reversed_4(self):  self.assertEqual(reversed([True, 3, "hat"]),["hat",3,True])
    def test_reversed_5(self):  self.assertEqual(reversed([1,1,1,2,1,1]),[1,1,2,1,1,1])
    def test_reversed_6(self):  self.assertEqual(reversed([1,3,5,2,4,6]),[6,4,2,5,3,1])
    def test_reversed_7(self):  self.assertEqual(reversed([5,5,5,5,5]),[5,5,5,5,5])
    def test_reversed_8(self):  self.assertEqual(reversed([1.1,2.2,3.3]),[3.3,2.2,1.1])
    def test_reversed_9(self):
        xs = [1,2,3,4,5,6]
        vs = reversed(xs)
        # the original list shouldn't be modified.
        self.assertEqual([1,2,3,4,5,6],xs)
    def test_reversed_10(self):
        # give test 9 more weighting.
        self.test_reversed_9()
    def test_reversed_11(self):
        xs = [1,2,3,4,5,6]
        id1 = id(xs) # memory location of the original list
        vs = reversed(xs)
        id2 = id(vs) # memory location of the returned list
        # they shouldn't match.
        self.assertFalse(id1==id2)
    def test_reversed_12(self):
        # give test 11 more weighting.
        self.test_reversed_11()
    def test_reversed_13(self):
        orig = [1,2,3,4,5]
        xs = orig[:]
        ans = reversed(xs)
        self.assertEqual(orig,xs)
        self.assertEqual(ans, [5,4,3,2,1])
    
    #---------------------------------------------------------------------
    
    def test_is_prime_01(self): self.assertFalse(is_prime(-5))
    def test_is_prime_02(self): self.assertFalse(is_prime(0))
    def test_is_prime_03(self): self.assertFalse(is_prime(1))
    def test_is_prime_04(self): self.assertTrue(is_prime(2))
    def test_is_prime_05(self): self.assertTrue(is_prime(3))
    def test_is_prime_06(self): self.assertFalse(is_prime(4))
    def test_is_prime_07(self): self.assertTrue(is_prime(5))
    def test_is_prime_08(self): self.assertTrue(is_prime(41))
    def test_is_prime_09(self): self.assertFalse(is_prime(117))
    def test_is_prime_10(self): self.assertTrue(is_prime(1117))
    def test_is_prime_11(self): self.assertTrue(is_prime(11117))
    def test_is_prime_12(self): self.assertTrue(is_prime(49999))
    def test_is_prime_13(self): self.assertFalse(is_prime(200000000))
    
    #---------------------------------------------------------------------
    
    def test_nub_01(self): self.assertEqual(nub([]),[])
    def test_nub_02(self): self.assertEqual(nub([5]),[5])
    def test_nub_03(self): self.assertEqual(nub([13,13,13]),[13])
    def test_nub_04(self): self.assertEqual(nub([1,2,3,1,2,3,1,2,3]),[1,2,3])
    def test_nub_05(self): self.assertEqual(nub([1,1,3,2,2,5,5,5,5,4]),[1,3,2,5,4])
    def test_nub_06(self): self.assertEqual(nub([1,4,2,5,3,1,2,3,4,5,6,1,3,2]),[1,4,2,5,3,6])
    def test_nub_07(self): self.assertEqual(nub([1,2,3,4,5]),[1,2,3,4,5])
    def test_nub_08(self): self.assertEqual(nub([-3,-1,1,3,2,4,-2,-4]),[-3,-1,1,3,2,4,-2,-4])
    def test_nub_09(self): self.assertEqual(nub([1,1,5,1,10,1,1,15,1,1]),[1,5,10,15])
    def test_nub_10(self): self.assertEqual(nub([1,2,3,2]),[1,2,3])
    def test_nub_11(self): self.assertEqual(nub([1,2,3,3]),[1,2,3])
    def test_nub_12(self): self.assertEqual(nub([1,2,3,1]),[1,2,3])
    def test_nub_13(self):
        orig = [1,2,3,1,2,3,1,2,3]
        xs = orig[:]
        ans = nub(xs)
        self.assertEqual(orig,xs)
        self.assertEqual(ans, [1,2,3])
    
    #---------------------------------------------------------------------
    
    def test_zip_with_1(self):
        def add(x,y): return x+y
        self.assertEqual(zip_with(add, [1,2,3,4], [10,10,10,10]),[11,12,13,14])
    def test_zip_with_2(self):
        def add(x,y): return x+y
        self.assertEqual(zip_with(add, [1,2,3,4], [5,6,7,8]),[6,8,10,12])
    def test_zip_with_3(self):
        def mul(x,y): return x*y
        # first list has fewer elements.
        self.assertEqual(zip_with(mul, [2,3,4],  [5,5,5,5,5]), [10,15,20])
    def test_zip_with_4(self):
        def mul(x,y): return x*y
        # second list has fewer elements.
        self.assertEqual(zip_with(mul, [2,3,4,5,6,7,8],  [5,5,5]), [10,15,20])
        def mul(x,y): return x*y
        # first list has zero elements.
        self.assertEqual(zip_with(mul, [],  [5,5,5,5,5]), [])
    def test_zip_with_5(self):
        def mul(x,y): return x*y
        # second list has zero elements.
        self.assertEqual(zip_with(mul, [2,3,4,5,6,7,8],  []), [])
    def test_zip_with_6(self):
        def mul(x,y): return x*y
        # both lists have zero elements.
        self.assertEqual(zip_with(mul, [],  []), [])
    
    def test_zip_with_7(self):  self.test_zip_with_1()
    def test_zip_with_8(self):  self.test_zip_with_2()
    def test_zip_with_9(self):  self.test_zip_with_3()
    def test_zip_with_10(self): self.test_zip_with_4()
    def test_zip_with_11(self): self.test_zip_with_5()
    def test_zip_with_12(self): self.test_zip_with_6()
    
    #---------------------------------------------------------------------
    
    def test_collatz_1(self):  self.assertEqual(collatz(1),[1])
    def test_collatz_2(self):  self.assertEqual(collatz(3),[3, 10, 5, 16, 8, 4, 2, 1])
    def test_collatz_3(self):  self.assertEqual(collatz(4),[4, 2, 1])
    def test_collatz_4(self):  self.assertEqual(collatz(5),[5, 16, 8, 4, 2, 1])
    def test_collatz_5(self):  self.assertEqual(collatz(10),[10, 5, 16, 8, 4, 2, 1])
    def test_collatz_6(self):  self.assertEqual(collatz(11),[11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1])
    def test_collatz_7(self):  self.assertEqual(collatz(17),[17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1])
    def test_collatz_8(self):  self.assertEqual(collatz(42),[42, 21, 64, 32, 16, 8, 4, 2, 1])
    def test_collatz_9(self):  self.assertEqual(collatz(100),[100, 50, 25, 76, 38, 19, 58, 29, 88, 44, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1])
    def test_collatz_10(self): self.assertEqual(collatz(8192),[8192, 4096, 2048, 1024, 512, 256, 128, 64, 32, 16, 8, 4, 2, 1])
    def test_collatz_11(self): self.assertEqual(collatz(2),[2,1])
    def test_collatz_12(self): self.assertEqual(collatz(6),[6,3,10,5,16,8,4,2,1])
    def test_collatz_13(self): self.assertEqual(collatz(99),[99, 298, 149, 448, 224, 112, 56, 28, 14, 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1])
    
    #---------------------------------------------------------------------
    
    def test_median_1 (self): self.assertEqual(median([1,1,1]),1)
    def test_median_2 (self): self.assertEqual(median([1,2,3]),2)
    def test_median_3 (self): self.assertEqual(median([1,2,2,3]),2)
    def test_median_4 (self): self.assertEqual(median([1,2,3,4]),2.5)
    def test_median_5 (self): self.assertEqual(median([1,3,2,3,1]),2)
    def test_median_6 (self): self.assertEqual(median([5,5,5,25]),5)
    def test_median_7 (self): self.assertEqual(median([13,6,13,3,7,29,12,1,2,14]),9.5)
    def test_median_8(self):
        orig = [5,5,5,25]
        xs = orig[:]
        ans = median(xs)
        self.assertEqual(orig,xs)
        self.assertEqual(ans, 5)
    
    #---------------------------------------------------------------------
    
    def test_mode_1 (self): self.assertEqual(mode([1,1,1]),[1])
    def test_mode_2 (self): self.assertEqual(mode([1,2,3]),[1,2,3])
    def test_mode_3 (self): self.assertEqual(mode([1,2,2,3]),[2])
    def test_mode_4 (self): self.assertEqual(mode([1,2,3,4]),[1,2,3,4])
    def test_mode_5 (self): self.assertEqual(mode([1,3,2,3,1]),[1,3])
    def test_mode_6 (self): self.assertEqual(mode([5,5,5,25]),[5])
    def test_mode_7 (self): self.assertEqual(mode([13,6,13,3,7,29,12,1,2,14]),[13])
    def test_mode_8(self):
        orig = [5,5,5,25]
        xs = orig[:]
        ans = mode(xs)
        self.assertEqual(orig,xs)
        self.assertEqual(ans, [5])
    
    #---------------------------------------------------------------------
    
    def test_check_sudoku_1(self):
        # a pretty regular pattern of a correct sudoku.
        grid = [[1,2,3, 4,5,6, 7,8,9],
                [4,5,6, 7,8,9, 1,2,3],
                [7,8,9, 1,2,3, 4,5,6],
                
                [2,3,4, 5,6,7, 8,9,1],
                [5,6,7, 8,9,1, 2,3,4],
                [8,9,1, 2,3,4, 5,6,7],
                
                [3,4,5, 6,7,8, 9,1,2],
                [6,7,8, 9,1,2, 3,4,5],
                [9,1,2, 3,4,5, 6,7,8],
               ]
        self.assertTrue(check_sudoku(grid))
    
    def test_check_sudoku_2(self):
        # keeps repeating the top three rows. only columns are faulty.
        grid = [[1,2,3, 4,5,6, 7,8,9],
                [4,5,6, 7,8,9, 1,2,3],
                [7,8,9, 1,2,3, 4,5,6],
                
                [1,2,3, 4,5,6, 7,8,9],
                [4,5,6, 7,8,9, 1,2,3],
                [7,8,9, 1,2,3, 4,5,6],
                
                [1,2,3, 4,5,6, 7,8,9],
                [4,5,6, 7,8,9, 1,2,3],
                [7,8,9, 1,2,3, 4,5,6],
                ]
        self.assertFalse(check_sudoku(grid))
    
    def test_check_sudoku_3(self):
        # keeps repeating the left three 3x3 groups. only rows are faulty.
        grid = [[1,2,3, 1,2,3, 1,2,3],
                [4,5,6, 4,5,6, 4,5,6],
                [7,8,9, 7,8,9, 7,8,9],
                
                [2,3,4, 2,3,4, 2,3,4],
                [5,6,7, 5,6,7, 5,6,7],
                [8,9,1, 8,9,1, 8,9,1],
                
                [3,4,5, 3,4,5, 3,4,5],
                [6,7,8, 6,7,8, 6,7,8],
                [9,1,2, 9,1,2, 9,1,2],
               ]
        self.assertFalse(check_sudoku(grid))

    def test_check_sudoku_4(self):
        # rows are shuffled to have only 3x3 groups wrong.
        grid = [[1,2,3, 4,5,6, 7,8,9],
                [2,3,4, 5,6,7, 8,9,1],
                [3,4,5, 6,7,8, 9,1,2],
                [4,5,6, 7,8,9, 1,2,3],
                [5,6,7, 8,9,1, 2,3,4],
                [6,7,8, 9,1,2, 3,4,5],
                [7,8,9, 1,2,3, 4,5,6],
                [8,9,1, 2,3,4, 5,6,7],
                [9,1,2, 3,4,5, 6,7,8],
               ]
        self.assertFalse(check_sudoku(grid))
        
    def test_check_sudoku_5(self):
        # only the extra 5 in the dead center is a problem.
        grid = [[1,2,3, 4,5,6, 7,8,9],
                [4,5,6, 7,8,9, 1,2,3],
                [7,8,9, 1,2,3, 4,5,6],
                
                [2,3,4, 5,6,7, 8,9,1],
                [5,6,7, 8,5,1, 2,3,4],
                [8,9,1, 2,3,4, 5,6,7],
                
                [3,4,5, 6,7,8, 9,1,2],
                [6,7,8, 9,1,2, 3,4,5],
                [9,1,2, 3,4,5, 6,7,8],
               ]
        self.assertFalse(check_sudoku(grid))
        
    def test_check_sudoku_6(self):
        # way wrong...
        grid = [[1,1,1, 1,1,1, 1,1,1],
                [1,1,1, 1,1,1, 1,1,1],
                [1,1,1, 1,1,1, 1,1,1],
                
                [1,1,1, 1,2,3, 1,1,1],
                [1,1,1, 4,5,6, 1,1,1],
                [1,1,1, 7,8,9, 1,1,1],
                
                [1,1,1, 1,1,1, 1,1,1],
                [1,1,1, 1,1,1, 1,1,1],
                [1,1,1, 1,1,1, 1,1,1],
               ]
        self.assertFalse(check_sudoku(grid))

    def test_check_sudoku_7(self):
        # another correct grid.
        grid = [[2,7,8, 4,6,9, 1,5,3],
                [6,9,3, 1,2,5, 4,7,8],
                [4,5,1, 7,8,3, 2,6,9],
                
                [7,6,5, 8,9,4, 3,2,1],
                [3,4,2, 6,1,7, 9,8,5],
                [8,1,9, 3,5,2, 6,4,7],
                
                [9,8,4, 5,3,6, 7,1,2],
                [5,2,6, 9,7,1, 8,3,4],
                [1,3,7, 2,4,8, 5,9,6],
               ]
        self.assertTrue(check_sudoku(grid))

    def test_check_sudoku_8(self):
        # another correct grid.
        grid =([[7,8,6, 4,9,3, 2,5,1],
                [3,2,1, 8,7,5, 4,9,6],
                [9,4,5, 6,2,1, 8,7,3],
                 
                [1,5,7, 9,3,8, 6,2,4],
                [2,9,8, 5,4,6, 3,1,7],
                [4,6,3, 7,1,2, 9,8,5],
                 
                [8,1,2, 3,6,7, 5,4,9],
                [6,7,4, 2,5,9, 1,3,8],
                [5,3,9, 1,8,4, 7,6,2],
             ])
        self.assertTrue(check_sudoku(grid))
    def test_check_sudoku_9(self): self.test_check_sudoku_1()
    def test_check_sudoku_10(self): self.test_check_sudoku_2()
    def test_check_sudoku_11(self): self.test_check_sudoku_3()
    def test_check_sudoku_12(self): self.test_check_sudoku_4()
    def test_check_sudoku_13(self):
        orig = [[2,4,1,9,7,5,3,8,6],
                [9,5,6,3,8,4,1,7,2],
                [3,8,7,1,6,2,5,9,4],
                [8,9,2,5,4,1,6,3,7],
                [6,1,5,8,3,7,4,2,9],
                [4,7,3,2,9,6,8,5,1],
                [1,2,9,6,5,3,7,4,8],
                [5,6,4,7,2,8,9,1,3],
                [7,3,8,4,1,9,2,6,5]]
        grid = orig[:]
        self.assertTrue(check_sudoku(grid))
        self.assertEqual(orig,grid)

    #---------------------------------------------------------------------

    ############################################################################
    
    # EC
#   def test_extra_credit_main_01(self):
#       self.assertEqual(ec(),2)
        
    ############################################################################
    
# This class digs through AllTests, counts and builds all the tests,
# so that we have an entire test suite that can be run as a group.
class TheTestSuite (unittest.TestSuite):
    # constructor.
    def __init__(self,wants):
        self.num_req = 0
        self.num_ec = 0
        # find all methods that begin with "test".
        fs = []
        for w in wants:
            for func in AllTests.__dict__:
                # append regular tests
                # drop any digits from the end of str(func).
                dropnum = str(func)
                while dropnum[-1] in "1234567890":
                    dropnum = dropnum[:-1]
                
                if dropnum==("test_"+w+"_") and (not (dropnum==("test_extra_credit_"+w+"_"))):
                    fs.append(AllTests(str(func)))
                    self.num_req += 1
                if dropnum==("test_extra_credit_"+w+"_") and not BATCH_MODE:
                    fs.append(AllTests(str(func)))
                    self.num_ec += 1
        
#       print("TTS ====> ",list(map(lambda f: (f,id(f)),fs)))
        # call parent class's constructor.
        unittest.TestSuite.__init__(self,fs)

class TheExtraCreditTestSuite (unittest.TestSuite):
    # constructor.
    def __init__(self,wants):
        self.num_req = 0
        self.num_ec = 0
        # find all methods that begin with "test_extra_credit_".
        fs = []
        for w in wants:
            for func in AllTests.__dict__:
                if str(func).startswith("test_extra_credit_"+w):
                    fs.append(AllTests(str(func)))
                    self.num_ec += 1
    
#       print("TTS ====> ",list(map(lambda f: (f,id(f)),fs)))
        # call parent class's constructor.
        unittest.TestSuite.__init__(self,fs)

# all (non-directory) file names, regardless of folder depth,
# under the given directory 'dir'.
def files_list(dir):
    this_file = __file__
    if dir==".":
        dir = os.getcwd()
    info = os.walk(dir)
    filenames = []
    for (dirpath,dirnames,filez) in info:
#       print(dirpath,dirnames,filez)
        if dirpath==".":
            continue
        for file in filez:
            if file==this_file:
                continue
            filenames.append(os.path.join(dirpath,file))
#       print(dirpath,dirnames,filez,"\n")
    return filenames

def test_main():
    global python_command
    if(platform.system() == 'Windows'):
        python_command = "python"
    else:
        python_command = "python3"

    if len(sys.argv)<2:
        raise Exception("needed student's file name as command-line argument:"\
            +"\n\t\""+python_command+" testerX.py gmason76_2xx_Px.py\"")
    
    if BATCH_MODE:
        print("BATCH MODE.\n")
        run_all()
        return
        
    else:
        want_all = len(sys.argv) <=2
        wants = []
        # remove batch_mode signifiers from want-candidates.
        want_candidates = sys.argv[2:]
        for i in range(len(want_candidates)-1,-1,-1):
            if want_candidates[i] in ['.'] or os.path.isdir(want_candidates[i]):
                del want_candidates[i]
    
        # set wants and extra_credits to either be the lists of things they want, or all of them when unspecified.
        wants = []
        extra_credits = []
        if not want_all:
            for w in want_candidates:
                if w in REQUIRED_DEFNS:
                    wants.append(w)
                elif w in SUB_DEFNS:
                    wants.append(w)
                elif w in EXTRA_CREDIT_DEFNS:
                    extra_credits.append(w)
                else:
                    raise Exception("asked to limit testing to unknown function '%s'."%w)
        else:
            wants = REQUIRED_DEFNS + SUB_DEFNS
            extra_credits = EXTRA_CREDIT_DEFNS
        
        # now that we have parsed the function names to test, run this one file.    
        run_one(wants,extra_credits)    
        return
    return # should be unreachable! 

# only used for non-batch mode, since it does the printing.
# it nicely prints less info when no extra credit was attempted.
def run_one(wants, extra_credits):
    
    has_reqs = len(wants)>0
    has_ec   = len(extra_credits)>0
    
    #wants = REQUIRED_DEFNS + SUB_DEFNS
    #extra_credits = EXTRA_CREDIT_DEFNS
    
    # make sure they exist.
    passed1 = 0
    passed2 = 0
    tried1 = 0
    tried2 = 0
    
    # only run tests if needed.
    if has_reqs:
        print("\nRunning required definitions:")
        (tag, passed1,tried1) = run_file(sys.argv[1],wants,False)
    if has_ec:
        print("\nRunning extra credit definitions:")
        (tag, passed2,tried2) = run_file(sys.argv[1],extra_credits,True)
    
    # print output based on what we ran.
    if has_reqs and not has_ec:
        print("\n%d/%d Required test cases passed (worth %.2f each)" % (passed1,tried1,weight_required) )
        print("\nScore based on test cases: %.2f/%.2f (%d*%.2f) " % (
                                                                passed1*weight_required, 
                                                                total_points_from_tests,
                                                                passed1,
                                                                weight_required
                                                             ))
    elif has_ec and not has_reqs:
        print("%d/%d Extra credit test cases passed (worth %.2f each)" % (passed2, tried2, weight_extra_credit))
    else: # has both, we assume.
        print("\n%d / %d Required test cases passed (worth %.2f each)" % (passed1,tried1,weight_required) )
        print("%d / %d Extra credit test cases passed (worth %.2f each)" % (passed2, tried2, weight_extra_credit))
        print("\nScore based on test cases: %.2f / %.2f ( %d * %.2f + %d * %.2f) " % (
                                                                passed1*weight_required+passed2*weight_extra_credit, 
                                                                total_points_from_tests,
                                                                passed1,
                                                                weight_required,
                                                                passed2,
                                                                weight_extra_credit
                                                             ))
    if BATCH_MODE:
        print("( %d %d %d %d )\n%s" % (passed1,tried1,passed2,tried2,tag))
    elif len(warnings) > 0:
        for warning in warnings:
            print("\nWARNING: "+warning)

# only used for batch mode.
def run_all():
        filenames = files_list(sys.argv[1])
        #print(filenames)
        
        results = []
        for filename in filenames:
            command = python_command+" \""+sys.argv[0]+"\" \""+filename+"\""
            print(" Batching on : " +filename + " (command: " + command + ")")
            # I'd like to use subprocess here, but I can't get it to give me the output when there's an error code returned... TODO for sure.
            lines = os.popen(command).readlines()
            
            # delay of shame...
            time.sleep(DELAY_OF_SHAME)
            
            name = os.path.basename(lines[-1])
            stuff = lines[-4].split(" ")[0:3:2]
            stuff += lines[-3].split(" ")[0:3:2]
            stuff += lines[-1].split(" ")[7:8]
            #print("STUFF: ",stuff, "LINES: ", lines)
            (passed_req, tried_req, passed_ec, tried_ec, total_points_from_tests) = stuff
            results.append((filename,int(passed_req), int(tried_req), int(passed_ec), int(tried_ec), float(total_points_from_tests)))
            
        print("\n\n\nGRAND RESULTS:\n")
        
            
        for (tag_req, passed_req, tried_req, passed_ec, tried_ec, total_points_from_tests) in results:
            name = os.path.basename(tag_req).strip()
            earned   = passed_req*weight_required + passed_ec*weight_extra_credit
            print("%20s : %3d / %3d = %5.2d %% (%d/%d*%d + %d/%d*%d)" % (
                                                            name,
                                                            earned,
                                                            total_points_from_tests, 
                                                            (earned/total_points_from_tests)*100,
                                                            passed_req,tried_req,weight_required,
                                                            passed_ec,tried_ec,weight_extra_credit
                                                          ))
# only used for batch mode.
def run_all_orig():
        filenames = files_list(sys.argv[1])
        #print(filenames)
        
        wants = REQUIRED_DEFNS + SUB_DEFNS
        extra_credits = EXTRA_CREDIT_DEFNS
        
        results = []
        for filename in filenames:
            # wipe out all definitions between users.
            for fn in REQUIRED_DEFNS+EXTRA_CREDIT_DEFNS :
                globals()[fn] = decoy(fn)
                fn = decoy(fn)
            try:
                name = os.path.basename(filename)
                print("\n\n\nRUNNING: "+name)
                (tag_req, passed_req, tried_req) = run_file(filename,wants,False)
                (tag_ec,  passed_ec,  tried_ec ) = run_file(filename,extra_credits,True)
                results.append((tag_req,passed_req,tried_req,tag_ec,passed_ec,tried_ec))
                print(" ###### ", results)
            except SyntaxError as e:
                tag = filename+"_SYNTAX_ERROR"
                results.append((tag,0,len(wants),tag,0,len(extra_credits)))
            except NameError as e:
                tag =filename+"_Name_ERROR"
                results.append((tag,0,len(wants),tag,0,len(extra_credits)))
            except ValueError as e:
                tag = filename+"_VALUE_ERROR"
                results.append((tag,0,len(wants),tag,0,len(extra_credits)))
            except TypeError as e:
                tag = filename+"_TYPE_ERROR"
                results.append((tag,0,len(wants),tag,0,len(extra_credits)))
            except ImportError as e:
                tag = filename+"_IMPORT_ERROR_TRY_AGAIN"
                results.append((tag,0,len(wants),tag,0,len(extra_credits)))
            except Exception as e:
                tag = filename+str(e.__reduce__()[0])
                results.append((tag,0,len(wants),tag,0,len(extra_credits)))
        
#           try:
#               print("\n |||||||||| scrupe: "+str(scruples))
#           except Exception as e:
#               print("NO SCRUPE.",e)
#           scruples = None
        
        print("\n\n\nGRAND RESULTS:\n")
        for (tag_req, passed_req, tried_req, tag_ec, passed_ec, tried_ec) in results:
            name = os.path.basename(tag_req)
            earned   = passed_req*weight_required + passed_ec*weight_extra_credit
            possible = tried_req *weight_required # + tried_ec *weight_extra_credit
            print("%10s : %3d / %3d = %5.2d %% (%d/%d*%d + %d/%d*%d)" % (
                                                            name,
                                                            earned,
                                                            possible, 
                                                            (earned/possible)*100,
                                                            passed_req,tried_req,weight_required,
                                                            passed_ec,tried_ec,weight_extra_credit
                                                          ))

def import_student_code(filename):
    
    # get code as string.
    f = open(filename)
    code = f.read()
    f.close()
    
    # using importlib to read contents into brand new module
    module_spec = importlib.util.spec_from_file_location("student",filename)
    module = importlib.util.module_from_spec(module_spec)
    module_spec.loader.exec_module(module)
    
    # register the module so it can be imported.
    sys.modules["student"] = module
    
    return module
    
def try_copy(filename1, filename2, numTries):
    have_copy = False
    i = 0
    while (not have_copy) and (i < numTries):
        try:
            # move the student's code to a valid file.
            shutil.copy(filename1,filename2)
            
            # wait for file I/O to catch up...
            if(not wait_for_access(filename2, numTries)):
                return False
                
            have_copy = True
        except PermissionError:
            print("Trying to copy "+filename1+", may be locked...")
            i += 1
            time.sleep(1)
        except BaseException as e:
            print("\n\n\n\n\n\ntry-copy saw: "+e)
    
    if(i == numTries):
        return False
    return True

def try_remove(filename, numTries, message):
    removed = False
    i = 0
    while os.path.exists(filename) and (not removed) and (i < numTries):
        try:
            os.remove(filename)
            removed = True
        except OSError:
            print(message)
            i += 1
            time.sleep(1)
    if(i == numTries):
        return False
    return True

def try_remove_test_file(file_name, function_name):
    try:
        try_remove(file_name, 3, "Trying to remove " + file_name)
    except:
        print("Warning: file " + file_name + 
            " couldn't be removed (possibly because " +
            "it was never closed in " + function_name + "(). " +
            "Please delete this file manually.")

def wait_for_access(filename, numTries):
    i = 0
    while (not os.path.exists(filename) or not os.access(filename, os.R_OK)) and i < numTries:
        print("Waiting for access to "+filename+", may be locked...")
        time.sleep(1)
        i += 1
    if(i == numTries):
        return False
    return True

# this will group all the tests together, prepare them as 
# a test suite, and run them.
def run_file(filename,wants=None,checking_ec = False):
    global total_points_from_tests
    
    if wants==None:
        wants = []
    
    # create an object that can run tests.
    runner = unittest.TextTestRunner()
    
    # define the suite of tests that should be run.
    #suite = None
    if not checking_ec:
        suite = TheTestSuite(wants)
        total_points_from_tests = suite.num_req*weight_required
    else:
        # do the same for the extra credit.
        suite = TheExtraCreditTestSuite(wants)
    
    # check number of tests before importing student code
    num_tests = suite.num_req + suite.num_ec
    
    # move the student's code to a valid file.
    #if(not try_copy(filename,"student.py", 5)):
    #   print("Failed to copy " + filename + " to student.py.")
    #   quit()
    
    # import student's code, and *only* copy over the expected functions
    # for later use.
    import importlib
    
    try:
        import_student_code(filename)
        import student
    except SyntaxError as e:
        print("SyntaxError in "+filename+":\n"+str(e))
        print("Run your file without the tester to see the details")
        return(filename+"_SYNTAX_ERROR",0,num_tests)
    except NameError as e:
        print("NameError in "+filename+":\n"+str(e))
        print("Run your file without the tester to see the details")
        return((filename+"_Name_ERROR",0,num_tests))    
    except ValueError as e:
        print("ValueError in "+filename+":\n"+str(e))
        print("Run your file without the tester to see the details")
        return(filename+"_VALUE_ERROR",0,num_tests)
    except TypeError as e:
        print("TypeError in "+filename+":\n"+str(e))
        print("Run your file without the tester to see the details")
        return(filename+"_TYPE_ERROR",0,num_tests)
    except ImportError as e:            
        print("ImportError in "+filename+":\n"+str(e))
        print("Run your file without the tester to see the details or try again")
        return((filename+"_IMPORT_ERROR_TRY_AGAIN   ",0,num_tests)) 
    except Exception as e:
        #traceback.print_last()
        print("Exception in loading"+filename+":\n"+str(e))
        print("Run your file without the tester to see the details")
        return(filename+str(e.__reduce__()[0]),0,num_tests)
    
    # make a global for each expected definition.
    for fn in REQUIRED_DEFNS+EXTRA_CREDIT_DEFNS :
        globals()[fn] = decoy(fn)
        try:
            globals()[fn] = getattr(student,fn)
        except:
            if fn in wants:
                print("\nNO DEFINITION FOR '%s'." % fn) 
    
    ans = runner.run(suite)
    num_errors   = len(ans.__dict__['errors'])
    num_failures = len(ans.__dict__['failures'])
    num_passed   = ans.__dict__['testsRun'] - num_errors - num_failures
    # print(ans)
    
    # remove our temporary file.
    if os.path.exists("__pycache__"):
        shutil.rmtree("__pycache__")
    
    tag = ".".join(filename.split(".")[:-1])
    
    
    return (tag, num_passed, num_tests)


# make a global for each expected definition.
def decoy(name):
        # this can accept any kind/amount of args, and will print a helpful message.
        def failyfail(*args, **kwargs):
            return ("<no '%s' definition was found - missing, or typo perhaps?>" % name)
        return failyfail

# this determines if we were imported (not __main__) or not;
# when we are the one file being run, perform the tests! :)
if __name__ == "__main__":
    test_main()
