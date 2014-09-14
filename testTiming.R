testTiming <- function (j,f) {
        ###
        ### argument j is the number of tests to run
        ### argument f is a csv file name to contain results
        ### a 1000*j by 1000*j square matrix is created, inverted,
        ###   cached, and retrieved for each test
        ### each test is timed and results written to f
        ### rows of f contain the test number, the matrix dimension,
        ###   and timings in seconds for the initial inversion calc 
        ###   and the subsequent cache retrieval of the inverted matrix
        ###
        ### initialize function list and observation data frame
        M<-makeCacheMatrix()
        tests<-data.frame(matrix(numeric(),0,4))
        ###
        ### run 100x100 matrix as a baseline
        M$set(matrix(rnorm(10000,2,2),100,100))
        message("Start baseline      ",date())
        tests<-rbind(tests,c(0,100,system.time(m<-cacheSolve(M))[[1]],
                system.time(m<-cacheSolve(M))[[1]]))
        colnames(tests)<-c("testNum","N","computeTime","retrieveTime")
        write.csv(tests,f,row.names=FALSE)       
        ###
        ### run tests
        for (i in 1:j) {
                message("Start test ",i,"        ",date())
                M$set(matrix(rnorm(i^2*1000000,2,2),i*1000,i*1000))
                tests<-rbind(tests,c(i,i*1000,
                        system.time(m<-cacheSolve(M))[[1]],
                        system.time(m<-cacheSolve(M))[[1]]))
                ###
                ### save results
                write.csv(tests,f,row.names=FALSE)
        }

}