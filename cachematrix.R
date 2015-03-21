##
## usage: z<-makeCacheMatrix(my_matrix)
##
## with this function we initialize the object list that holds the matrix manipulation/access functions
##
## this function contains the subfunctions that store and retrieve the original matrix and the cache matrix


makeCacheMatrix <- function(x = numeric()) {
        s <- NULL							#when creating the object we clear cache matrix 's'

        set <- function(y) {					#the set method will store the matrix in 'x'
                x <<- y
                s <<- NULL					#if we set a new matrix, we clear the cache 
        }

        get <- function() x					#the get method will retrieve the matrix stored in 'x'

        setsolve <- function(solve) s <<- solve		#setsolve stores the inverse square of the matrix as matrix 's'

        getsolve <- function() s				#getsolve retrieves the calculated inverse square matrix in 's' (cached)

        list(set = set, get = get,				#we create a list of objects from the functions above
             setsolve = setsolve,
             getsolve = getsolve)
}


##
## usage cacheSolve(z)
##
## this function checks if the cache matrix is set,
##
## this function will check whether the cache is already set.
## if yes, instead of recomputing the matrix, the cached value is returned, with a message indicating that the cached value was returned
## if the cache is empty, the inverse square matrix is computed with solve function, and the result is stored in the cache

cacheSolve <- function(x, ...) {
        s <- x$getsolve()					#we look up the value of cache inverse square matrix 's'

        if(!is.null(s)) {					#if it's NOT NULL, it is retrieved ('s')
                message("getting cached data")
                return(s)					#we return cached 's' and end our function 
        }

        data <- x$get()						#if previous condition was NULL we retrieve the original matrix 'x'
        s <- solve(data, ...)					#and we compute the inverse square matrix with the solve function
        x$setsolve(s)						#we store the the computed inverse square matrix in 's'
        s								#and we also return it
}

