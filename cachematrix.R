##This function involves 2 parts: i. create a matrix from user input and  cache its inverse, ii. compute inverse of the matrix retrieved from user input; if already in cache and matrix not changed, return the cache value 

## This function create a matrix from user input and cache its inverse
makeCacheMatrix <- function(x = matrix()) {
			#Initialize mat object
			mat <- NULL
			#Replace the existing matrix with the new one
            set <- function(y) {
                    x <<- y
                    mat <<- NULL
            }
			#Retrieve existing matrix
			get <- function() x
			#Assign result of an inverse calculation to another object
			setInverse <- function(inv) mat <<- inv
			#Retrieve inverted matrix from cache
			getInverse <- function() mat
			#Creating tag with the name similar to the respective function
			list(set = set, get = get,
				 setInverse = setInverse,
				 getInverse = getInverse)
			
}


## This function compute the inverse of the matrix from user input, and return the cached value if the same matrix has been inverted and cached

cacheSolve <- function(x, ...) {
        #Check if the inverted matrix of the input is available in cache and return the value
		mat <- x$getInverse()
            if(!is.null(mat)) {
                    message("getting cached data")
                    return(mat)
				}	
		#Invert the new matrix, save it to object "mat"  
		data <- x$get()
        mat <- solve(data, ...)
        x$setInverse(mat)
        mat
}
