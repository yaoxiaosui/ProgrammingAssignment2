#Here is a constructor that make the "special cached" matrix

makeCacheMatrix <- function(x = matrix()){
  m<- NULL #a matrix that used to cache 
  
  #getter function
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  #getter function
  get <- function(){
    x
  }
  
  #helper function that help set the inverse matrix
  setinverse <- function(inverse_matrix){
    m <<- inverse_matrix
  }
  
  #helper function that help get the inverse matrix
  getinverse <- function(){
         m
  }
  list(set = set, get= get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#a function that quickly solve the inverse of a matrix
cacheSolve <- function(x,...){
  
  m <- x$getinverse() #first try to see if the matrix exsits
  if(!is.null(m)){
    message("successfully get cached matrix")
    return(m)
    #if the matrix does not change, simply return the cached matrix
  }
  temp <- x$get()
  m <- solve(temp, ...) #if not, solved the matrix
  x$setinverse(m) #set the inverse matrix
  m
}
