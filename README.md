### Assignment-Programming-Assignment-2-Lexical-Scoping

### makeCacheMatrix

             makeCacheMatrix<- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                x <<- y
                m <<- NULL
                }
                get <- function() x
                setreverse <- function(reverse) m <<- reverse ##  solver not Solve
                getreverse <- function() m   
                list(set = set, get = get,
                setreverse = setreverse,
                getreverse = getreverse)
          }

### make matrix v
 
          v<- makeCacheMatrix( matrix( c(2, 4, 3, 1),nrow=2, ncol=2,byrow = TRUE) )
          
          
### make cachesolve



        cacheSolve<- function(x, ...) {
            m <- x$getreverse()
            if(!is.null(m)) {
            message("getting cached data")
            return(m)
            }
           data <- x$get()
           m <- solve(data, ...)
           x$setreverse(m)
           m
           }
 
### Some operations

         v$get()

         v$getreverse()

         cacheSolve(v)

         v$getreverse

         cacheSolve(v)

         v$getreverse()

         x <- cacheSolve(v)

         x
 

 



