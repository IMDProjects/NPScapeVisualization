# This function fetches population density data from the NPScape SQL suerver
# Note that the Unit, AOA, and Year parameters are optional; if omitted, all possible values are included
# parameters:
#    mdb:   the open connection to the SQL server
#    Unit:  a vector of 1 or more unit codes
#    AOA:    a vector of one or more of {'park','3km','30km'}
#    Year:   a vector of one or more of {1990,2000,2010}

fetch_pdd <- function(mdb=odbcNPScape,
                      Unit=NULL, 
                      AOA=NULL,
                      Year=NULL) {

    if (is.null(AOA)) AOA <- c('Park','3km','30km')                  
    if (is.null(Year)) Year <- c('1990','2000','2010')
    # build list of tables to query
    tmp <- expand.grid(AOA,Year)
    tmp$TabName <- paste("pddptt",tmp$Var2,"_",tmp$Var1,"_stats",sep='')
    
    if (is.null(Unit)) {
        hddQuery <- paste("SELECT *, '",tmp$Var1, "' AS AOA FROM ", tmp$TabName,sep="")
    } else {
        hddQuery <- paste("SELECT *, '",tmp$Var1, "' AS AOA FROM ", tmp$TabName, " WHERE AOA_NAME IN ('",paste(Unit,sep='',collapse="','"),"') ",sep="")
    }
    bigQuery <- paste(hddQuery," UNION ALL ",sep=' ',collapse='')
    Z <- nchar(bigQuery)
    bigQuery <- paste(substr(bigQuery,1,Z-11),';',sep='') # strip off extra UNION ALL and add ;
    flag <- try(sqlQuery(mdb,bigQuery,stringsAsFactors=FALSE,as.is=TRUE))
    if (inherits(flag,'try-error')) cat('SQL failure, \n')
    return(list(Result=flag,SQL=bigQuery))
} # bottom function fetch_pdd
