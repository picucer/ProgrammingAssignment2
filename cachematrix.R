
# I apologize for my English!
# The following two functions define a new type of invertible matrix. 
# Invert a matrix is computational costly, so the solve() function will be invoked 
# only if the inverse of the matrix doesn't be computed yet. 
# Otherwise, a cached version of the inverted matrix will be returned.

# makeCacheMatrix function is similar to a class definition of other languages. 
# It has several atributes and methods.This function stores the matrix (x) and its inverted matrix (ix) 
# and implements the methods to set and get their values.
 
# the "class atributes" are: 
# x: an invertible matrix 
# ix: inverse of x
# The methods are:
# get: return the "non inverted" matrix
# getInverted: return the inverted matrix
# set: store a matrix "anInvertibleMatrix"
# setInverted: store the inverse matrix
# list: populate the methods

makeCacheMatrix <- function(x = matrix()) {
	ix<-NULL							#ix is de inverse of x
	get <- function() x
	getInverted <- function() ix
	set <- function(anInvertibleMatrix){
		x <<- anInvertibleMatrix
		ix <<- NULL						#RESET ix to NULL to know the matrix x has changed
	}
	setInverted<-function(inverted){
		ix <<- inverted
	}
	list(set = set, get = get,
		 setInverted = setInverted,
		 getInverted = getInverted
		 )	
}



# cacheSolve invoke the solve() function only if the inverse matrix hasn't been computed yet, 
# or the "non inverted" matrix has changed using the set() method. 
# The result is stored in the ix attribute of the makeCacheMatrix object
# Otherwise, return the cached version of the inverse matrix.
# For testing purpose this function prints the inverted matrix and some messages and return the inverted matrix too.
 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverted <- x$getInverted()
		if(!is.null(inverted)){
			message("using de cached version of the inverse")
		}else{
			message("Calculating the inverse of the matrix")
			inverted <- solve(x$get())
			x$setInverted(inverted)
		}
		print (inverted)
		return (inverted)
}