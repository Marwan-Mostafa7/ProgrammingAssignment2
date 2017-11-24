## Computing inverse of a long matrix once, and saving this data
## in the cashe memory to save doing time of computation that have been done before
## Assuming the Data of matrix is not changing



##  matrix  -> list containing a function to

#1) set the value of the vector
#2) get the value of the vector
#3) set the value of the mean
#4) get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  
  
  # cashed inverse initialized to null
  cashed_inv <- NULL
  
  
  set_matrix <- function(y){
    x <<- y
    cashed_inv <<- NULL
  }
  
  get_matrix <- function() x
  
  set_inverse_mat <- function(calc_inverse) cashed_inv<<-calc_inverse
  get_inverse_mat <- function() cashed_inv
  
  list(set=set_matrix, get=get_matrix, setinverse=set_inverse_mat, getinverse=get_inverse_mat)
}



# matrix -> matrix

## Computes the inverse of the consumed matrix
## if inverse is already exist in cashe the saved data is directly returned
## without further computaions

cacheSolve <- function(x, ...) {
  
  
  inv <- x$getinverse()
  
  ## Checking if value is in cache memory or not
  if(!is.null(inv)){
    print("Computed from Cashe")
    return (inv)
  }
  
  data <- x$get()
  
  inv <- solve(data)
  
  x$setinverse(inv)
  
  return(inv)
}
