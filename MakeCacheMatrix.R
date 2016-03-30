##MakeCacheMatrix creates a matrix that ill cache so you can call it again

 makeCacheMatrix<- function(x=matrix()) {
        y<-NULL
        set<-function(t) {
                 x<<-t
                 y<<-NULL
             }
             get<-function()x
             setmatrix<-function(matrix) y<<-inverse
             getmatrix<-function() y
             list(set=set,get=get,
                            setmatrix=setmatrix,
                            getmatrix=getmatrix)
        
         }

 ## This function computes the inverse of the  "matrix" created by 
 ## makeCacheMatrix above. If the 
 ## matrix has not changed), then it should retrieve it from the cache
 
 
cacheSolve<- function(x,...) {
             y<-x$getmatrix()
             if(!is.null(y)) {
                    message("Getting cached data.")
                     return(y)
                 }
                data<-x$get()
                 y<-solve(data)
                 x$setmatrix
             y
             }