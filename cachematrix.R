## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The following function is to create a Cache Matrix which takes the input
#SetM creates the matrix, GetM fetches the output of the matrix, SetI sets the inverse of the matrix while GetI gets the Inverse of the Matrix
makeCacheMatrix <- function(x = matrix()) 
{
                invM <- NULL
		SetM <- function (y)
		{
			x <<-y
			invM <<- NULL
		}
		GetM <- function() x
		SetI <- function(inverse) InvM <<- inverse
		GetI <- function() InvM
		list(SetM = SetM, GetM = GetM, SetI = SetI, GetI = GetI)

}


## Write a short comment describing this function
#CacheSolve is used to check if the Inverse of the matrix is already calculated. 
#If it is, then this function fetches the inverse instead of calculation it all over again
cacheSolve <- function(x, ...) 
{        ## Return a matrix that is the inverse of 'x'
        InvM <- x$GetI
		if(!is.null(InvM))
		{
			message("getting Cached Inverse")
			return(InvM)
		}
		data <- x$GetM()
		InvM <- inverse(data, ...)
		x$SetI(InvM)
		InvM
}
