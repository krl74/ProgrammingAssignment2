## Put comments here that give an overall description of what your
## functions do
## Im trying to follow the arguments described in the article posted in the week3 forum: 
##https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md

## Write a short comment describing this function: it creates a "special matrix", 
## like the special vector in the example, that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y) {
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setInverse<- function(inverse) inv <<-inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function: this function computes the inverse of the makecachematrix
## using the solve function. If the inverse has already been calculated and not changed it should retrieve it
## from the cache along the lines described in the vector example
## If you can explain why this type of functionality isnt already done in the L1,L2,L3 cache memory on the pc
## please comment when you review thank you

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
## To test first set a matrix for example my_matrix<-makeCacheMatrix(matrix(1:4, 2, 2))
## my_matrix$get() to display the contents
## my_matrix$getInverse() will display NULL as we havn't actually cached any inverse data yet(from cacheSolve)
## run cacheSolve(my_matrix) to inverse data and display it(and cache it)
## if you run my_matrix$getInverse() again now it will display the inverse data rather than NULL
## as it now gets it from the enviroment wihtin the function designed (if I understand this whole thing correct)