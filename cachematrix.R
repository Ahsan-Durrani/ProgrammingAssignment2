#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix())
{
        Invm <- NULL
        set <- function(s)
        {
                m <<- s
                Invm <<- NULL
        }
        get <- function() m
        setmatrix <- function(solve) Invm <<- solve
        getmatrix <- function() Invm
        list(set=set,get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(m,...)
{
        mat <- m$getmatrix()
        if(!is.null(mat))
        {
                message("getting cached data")
                return(mat)
        }
        Inverse <- m$get()
        mat <- solve(Inverse, ...)
        m$set(mat)
        mat
}
