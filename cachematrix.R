## The main idea was taken from the example in description 
## of ProgrammingAssignment2 (README.md)


## Function makeCacheMatrix() takes square matrix as input argument

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Function cacheSolve takes a special "matrix" returned by 
## makeCacheMatrix as argument and returns a matrix that is the inverse of it
## If inverse matrix is already exists it prints a message and 
## returns a stored value. 

cacheSolve <- function(x, ...) {
        
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
