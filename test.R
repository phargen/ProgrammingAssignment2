# test script for cacheMatrix
cm <- makeCacheMatrix(matrix(c(6,-7,0,4), 2, 2, byrow = TRUE))
print(cm$get())
#       [,1] [,2]
# [1,]    6   -7
# [2,]    0    4
print(cacheSolve(cm))
#         [,1]      [,2]
# [1,] 0.1666667 0.2916667
# [2,] 0.0000000 0.2500000
print(cacheSolve(cm))
# getting cached data
#         [,1]      [,2]
# [1,] 0.1666667 0.2916667
# [2,] 0.0000000 0.2500000
cm$set(matrix(c(6,-7,0,4), 2, 2))
print(cm$get())
#         [,1] [,2]
# [1,]    6    0
# [2,]   -7    4
print(cacheSolve(cm))
#         [,1] [,2]
# [1,] 0.1666667 0.00
# [2,] 0.2916667 0.25
print(cacheSolve(cm))
# getting cached data
#         [,1] [,2]
# [1,]    6    0
# [2,]   -7    4
cacheSolve(makeCacheMatrix(matrix(c(1,1,1,1), 2, 2)))
# Error in solve.default(data, ...) : 
#     Lapack routine dgesv: system is exactly singular: U[2,2] = 0
cacheSolve(makeCacheMatrix(matrix(c(1, 2, 0, -2, 1, 0), 2, 3)))
# Error in solve.default(data, ...) : 'a' (2 x 3) must be square 
cacheSolve(matrix(c(6,-7,0,4), 2, 2))
# Error in cacheSolve(matrix(c(6, -7, 0, 4), 2, 2)) : 
#     Invalid use of cacheSolve - must pass in cacheMatrix object created by makeCacheMatrix(...)