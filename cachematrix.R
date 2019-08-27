makeCacheMatrix <- function(x = matrix()){
  m<- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function(){
    x
  }
  
  setinverse <- function(inverse_matrix){
    m <<- inverse_matrix
  }
  
  getinverse <- function(){
         m
  }
  list(set = set, get= get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x,...){
  
  m <- x$getinverse()
  if(!is.null(m)){
    message("successfully get cached matrix")
    return(m)
  }
  temp <- x$get()
  m <- solve(temp, ...)
  x$setinverse(m)
  m
}
