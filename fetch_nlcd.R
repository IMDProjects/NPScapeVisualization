##############################################################################
# This function pulls requested nlcd data from the NPScape SQL server
# parameters:
#    Unit:  a vector of 1 or more unit codes
#    metric: a vector of one or more of {'lac1','lac2','lnc'}
#    AOA:    a vector of one or more of {'park','3km','30km'}
#    Year:   a vector of one or more of {2001,2006,2011}

fetch_nlcd <- function(Unit, metric, AOA, Year=2011) {
    # crosswalk Year to first part of table names
    YT <- data.frame(Year=c(2001,2006,2011),
                     Prefix=c('nlcd2001v11','nlcd2006v11','nlcd2011')
  					)
    # deal with possibly more than one Year, unit, or AOA
	Prefixes <- YT$Prefix[YT$Year%in%Year]
	tmp <- expand.grid(Prefixes,metric,AOA)
	tmp <- merge(tmp,YT,by.x='Var1',by.y='Prefix',all.x=TRUE)
    TabName <- paste(tmp[,1],tmp[,2],'stats',tmp[,3],sep='_') # AOA_NAMEs
	ImageName <- tmp$Year
    tmp2 <- expand.grid(Unit,AOA)
    AOAname <- paste(tmp2[,1],'_',tmp2[,2],sep='')
    rm(tmp)

    
    hddQuery <- paste("SELECT *, '",ImageName, "' AS Image FROM ", TabName, " WHERE AOA_NAME IN ('",paste(AOAname,sep='',collapse="','"),"') ",sep="")
    bigQuery <- paste(hddQuery," UNION ALL ",sep=' ',collapse='')
    Z <- nchar(bigQuery)
    bigQuery <- paste(substr(bigQuery,1,Z-11),';',sep='') # strip off extra UNION ALL and add ;
    # selecting from any of the AOAnames when we know the AOA from the tablename is slower
	# but saves a great deal of additional coding that would become finicky.  Sue me.
    flag <- try(sqlQuery(odbcNPScape,bigQuery,stringsAsFactors=FALSE,as.is=TRUE))
    if (inherits(flag,'try-error')) cat('SQL failure, \n')
    return(flag)
} # bottom function fetch_nlcd
