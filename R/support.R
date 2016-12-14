#' Main function for calculating beta-diversity (partionining it into replacement and richness components)
#' function modified from BAT package
#'  
#' @ comm = matrix containing community dataset
#' @ abund=FALSE if abundance data not present
#' @ func = dissimilarity function to be used
#' 

betaObs <- function(comm,abund = FALSE, func = "jaccard"){
    if(sum(comm) == 0)                                ##if no species on any community return 0
        return(list(Btotal = 0, Brepl = 0, Brich = 0))
    if (!abund || max(comm) == 1) {										##if incidence data
        obs1 <- sobs(comm[1,,drop=FALSE])
        obs2 <- sobs(comm[2,,drop=FALSE])
        obsBoth <- sobs(comm)
        a <- obs1 + obs2 - obsBoth
        b <- obsBoth - obs2
        c <- obsBoth - obs1
    } else if (abund){								##if abundance data
        a <- 0
        b <- 0
        c <- 0
        for (i in 1:ncol(comm)){
            minComm <- min(comm[1,i], comm[2,i])
            a <- a + minComm
            b <- b + comm[1,i] - minComm
            c <- c + comm[2,i] - minComm
        }
    }
    denominator <- a + b + c
    if(tolower(substr(func, 1, 1)) == "s")
        denominator <- denominator + a
    return(list(Btotal = (b+c)/denominator, Brepl = 2*min(b,c)/denominator, Brich = abs(b-c)/denominator))
}


#' Number of columns where abundance is higher than 0 (from BAT package)
#'  
#' @ comm = matrix containing community dataset
#' 

sobs <- function(comm){
        return(length(colSums(comm)[colSums(comm) > 0]))
}

