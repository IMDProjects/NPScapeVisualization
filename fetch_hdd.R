##############################################################################
# function to retrieve desired data from the NPScape SQL Server hdd housing density files
# parameters:
#    Unit:   a vector of 1 or more unit codes
#    AOA:    a vector of 1 or more of {'Park','3km",'30km'}
#    Start:  the first decade requested (defaults to the minimum of 1970)
#    End:    the last decade requested, out to 2100, defaults to 2030

fetch_hdd <- function(Unit, AOA, Start=1970, End=2030) {
    Year <- seq(from=Start,to=End,by=10)
    tmp <- expand.grid(Year,AOA)
    TabName <- paste('hdd_',tmp[,1],'_stats_',
                     tmp[,2],sep='')
    tmp <- expand.grid(Unit,AOA)
    AOAname <- paste(tmp[,1],'_',tmp[,2],sep='')
    rm(tmp)

    
    hddQuery <- paste("SELECT AOA_NAME, DECADE, AOA_FEATURE_AREA_SQKM, TAREA_SQKM, ",
                  "AREA_SQKM, PCT_AREA, VALUE_, COUNT_, CLASSNAME ",
                  "FROM ", TabName, " WHERE AOA_NAME IN ('",paste(AOAname,sep='',collapse="','"),"') ",sep="")
    bigQuery <- paste(hddQuery," UNION ALL ",sep=' ',collapse='')
    Z <- nchar(bigQuery)
    bigQuery <- paste(substr(bigQuery,1,Z-11),';',sep='')
    
    flag <- try(sqlQuery(odbcNPScape,bigQuery,stringsAsFactors=FALSE,as.is=TRUE))
    if (inherits(flag,'try-error')) cat('SQL failure, \n')
    return(flag)
} # bottom function fetch_hdd
