makeCacheMatrix <- function(x = matrix()) {
  #create a variable for the inverse first
  i <- NULL
  #have functions for cacheSolve to call
  get <- function() x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  getInverse <- function() i
  setInverse <- function(inverse) i <<- inverse
  list(get = get, getInverse = getInverse, setInverse = setInverse)
}

cacheSolve <- function(x, ...) {
  #create a local variable for the input "special vector"
	y <- x 
  
	if(identical(x,y)){ #if the matrix has not been changed
		cacheInverse <- x$getInverse()
		if(!is.null(cacheInverse)) {
			message("getting cached data")
			return(cacheInverse)
		}else{
			matrix <- x$get() #get the entire matrix out
			inverse <- solve(matrix) #get the matrix inverse and set it
			x$setInverse(inverse)
			return(inverse)
		}
	}else{#the matrix has been changed. reassign the values for its cache matrix and then get the inverse
		cache1$set(y)
		newMatrix <- cache1$get()
		newInverse <- solve(newMatrix)
		newMatrix$setInverse(newInverse)
		return(newInverse)
	}
}