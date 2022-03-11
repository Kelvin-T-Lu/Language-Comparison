# Kelvin Lu  
# CS 463-001 HW1
def fib(index):  # O(index)
    # Checks if the index is the first two index.
    # If index is not first two index
    #   Add the last two computed values pairs until index is reached.
    #   Each pair will be stored iteratively.

    # Check if index argument are the first two number in fib seqeunce.
    if index == 0:
        return 0
    if index == 1:
        return 1

    # Storage variables for the addition seqeunce.
    element = 0  # The storage variable
    firstNum = 0  # The first number in the sequence to be added.
    secondNum = 1  # The second (last) number to be added from the sequence.

    # Enumerate through the index until index is reached
    # Adds the value iteratively until finish.
    for x in range(1, index):
        element = firstNum + secondNum
        firstNum = secondNum
        secondNum = element
        # print("This is the first value: ",  firstNum)
        # print("This is the second value: ",  secondNum)

    return element


def reversed(list):  # O(n)
    # Creates a new list
    # Incrementally append reverse value using python's negative indices.
    newList = []
    for x in range(len(list)):
        newList.append(list[-(x+1)])
    return newList


def is_prime(n):  # O(n)
    # If number is < 2 , number is not prime by defintion.
    # Uses a loop to increment all possible multiples until n.
    #   If n%i == 0, multipel found, return False.
    #   Return True otherwise.
    if n < 2:
        return False

    # Finding all possible multiples of n.
    for i in range(2, n):
        if n % i == 0:
            return False
    return True


def nub(list):  # O(len(list))
    # Creates a newList
    # Adds elements that's not in the newList from list.
    newList = []
    for x in range(len(list)):
        if list[x] not in newList:
            newList.append(list[x])

    return newList


def zip_with(f, xs, yx):  # O(n)
    # Creates a tuple with both list and elements with Zip function.
    # Creates an output list.
    # Add the output of f function with the tuple as parameters.
    zipTuple = zip(xs, yx)

    output = []
    # Iterate through tuple and place it into f function.
    # Add the output into the list.
    for i in zipTuple:
        output.append(f(i[0], i[1]))
    return output


def collatz(n):
    # Creates an empty list and calls the collatz_helper.
    list = []
    return collatz_helper(n, list)


def collatz_helper(n, list):
    # Append the current number to list.
    # Checks the property of n, recursively call collatz_helper until done.
    list.append(n)
    if n == 1:
        return list
    elif isEven(n):
        return collatz_helper(n/2, list)
    else:
        return collatz_helper(n*3+1, list)


def median(xs):  # O(1)
    # Record the middleIndex of the list.
    # Sort the list.
    # If the length of the list is odd, return element at middleIndex.
    # Else return average of the two middleIndexes.
    middleIndex = len(xs)//2
    xs.sort()

    if (not isEven(len(xs))):
        return xs[middleIndex]
    else:
        return (xs[middleIndex-1] + xs[middleIndex])/2


def mode(xs):  # O(len(xs)
    # Creates a dictionary for the key element and value occurence.
    # Stores the key and occurence in the map.
    # Find the max occurence and append the most occured number in list.
    map = {}  # Key: Element in list ; Value : Occurrence in List
    for elements in xs:
        if elements not in map:
            map[elements] = 1
        else:
            map[elements] += 1

    maxOccurrence = 0
    output = []
    for i in map.keys():
        if map[i] > maxOccurrence:
            maxOccurrence = map.get(i)
            output.clear()
            output.append(i)
        elif map[i] == maxOccurrence:
            output.append(i)

    return output


def check_sudoku(grid):  # O(n^2)
    # Check all values are within range
    # Check each rows
    # Check each columns
    # Check each 3x3 block

    # Individually checks all value is within range.
    for row in grid:
        for element in row:
            if element > 9 or element < 0:
                return False

    # Check if all rows are valid.
    if not check_rows(grid):
        return False

    # Check if all columns are valid.
    if not check_col(grid):
        return False

    # Check if all 3x3 subsquares are valid.
    if not check_blocks(grid):
        return False
    return True


def check_rows(grid):
    # Takes the sum of each row in grid.
    # If it's not equal to the limit, row is off.
    # Use of set to make sure all digits are unique.
    limit = 45
    setLimitSize = len(grid)
    # Check sum of each row and make sure each value is unique.
    for row in grid:
        sum = 0
        my_set = set()
        for element in row:
            sum += element
            my_set.add(element)
        if sum != limit or len(my_set) != setLimitSize:
            return False
    return True


def check_col(grid):
    # Define the variables.
    # For each column, add all of the number in the row indices.
    # If it doesn't add up to limit, return False.
    # Otherwise, return True.
    # Use of set to make sure all digits are unique.

    # Boundaries
    limit = 45
    setLimitSize = len(grid[0])
    # Check sum of every column and each value is unique.
    for col in range(len(grid[0])):
        sum = 0
        my_set = set()
        for row in range(len((grid))):
            sum += grid[row][col]
            my_set.add(grid[row][col])
        if sum != limit or len(my_set) != setLimitSize:
            return False
    return True


def check_blocks(grid):
    # Define the limit variables.
    # Add a indices scale to represent each 3x3 block.
    # Use of a set to ensure unique value.
    # If len(set) is not the setLimit or sum is not the sumLimit, return False
    # Otherwise, Return true.

    # Boundaries
    sumLimit = 45
    setLimit = len(grid)  # ASSUMPTION - Grid is a square

    # Get the row and col indicies of each subquares.
    #   Add the subsquare's sum.
    #   Ensure all subsquare's values are unique with set.
    for scale in [0, 3, 6]:  # Scale to get subsquare.
        sum = 0
        my_set = set()
        for row in [0, 1, 2]:  # Row indicies in subsquare.
            for col in [0, 1, 2]:  # Col indecies in subsquare.
                my_set.add(grid[row+scale][col+scale])
                sum += grid[row+scale][col+scale]
        if (len(my_set) != setLimit) or (sum != sumLimit):
            return False
    return True


def isEven(n):  # O(1)
    # Returns the boolean expression if n is even.
    if n % 2 == 0:
        return True
    return False
