setWindowTitle(paste('NPScape Database  PDD Figures',R.version$arch))
# setwd('f:/NPScape/AutoFigures')
pkgList <- c("RODBC","grid","gridExtra","ggplot2",'beepr','devtools')
inst <- pkgList %in% installed.packages()
if (length(pkgList[!inst]) > 0) install.packages(pkgList[!inst])
lapply(pkgList, library, character.only = TRUE)

GitDir <- 'f:/Git/NPScapeVisualization'

source(paste(GitDir,'fetch_nlcd.R',sep='/'))
source(paste(GitDir,'AutoFigure_nlcd.R',sep='/'))


NPScapeConnectString <- paste("Driver=SQL Server; ",
                              "Server=INP2300FCSWUPA5\\GISDATA; ",
                              "uid=NPScape_Viewer;pwd=viewer2013…;",
                              sep='')
                              
#INP2300FCSWUPA5\GISDATA                              
 odbcNPScape <- odbcDriverConnect(NPScapeConnectString)

TableList <- sqlTables(odbcNPScape)[2:4]
TableList <- TableList$TABLE_NAME[TableList$TABLE_SCHEM=='dbo']
# n.b. as of July 2014, the only views (stored queries) are system views
#  If user stored queries are developed, they should still have 'db' as TABLE_SCHEM


##############################################################################
# Grab Utility Tables
luUnits <- sqlFetch(odbcNPScape,'tlu_NPS_Units',
                    as.is=TRUE,stringsAsFactors=FALSE)
str(luUnits)

#luNPScapeUnits <- sqlFetch(odbcNPScape,'tlu_NPScape_Units',
#                           as.is=TRUE,stringsAsFactors=FALSE)
#str(luNPScapeUnits)
#luIMunits <- sqlFetch(odbcNPScape,"tlu_IM_Units",
#                      as.is=TRUE,stringsAsFactors=FALSE)
#str(IMunits) # lookup table of codes & names
#xParkNetwork <- sqlFetch(odbcNPScape,'xref_Parks_Networks',
#                         as.is=TRUE,stringsAsFactors=FALSE)
#str(xParkNetwork)
#xParkRegion <- sqlFetch(odbcNPScape,'xref_Parks_NPS_Regions',
#                         as.is=TRUE,stringsAsFactors=FALSE)
#str(xParkRegion)

xNPS_FWS_LCC <- sqlFetch(odbcNPScape,'xref_NPS_FWS_LCC',
                         as.is=TRUE,stringsAsFactors=FALSE)
str(xNPS_FWS_LCC)
xPNR <- sqlFetch(odbcNPScape,'tlu_IMParksNetworksRegions',
                         as.is=TRUE,stringsAsFactors=FALSE)
str(xPNR)
AOAlist <- c('Park','3km','30km')



##############################################################################
# source('fetch_hdd.R')
# source('fetch_nlcd.R')
# source('fetch_pdd.R')
# source('AutoFigure_hdd.R')

TestUnits <- c('GOGA','SAMO','CABR','SAGU','PETR','AUTO','TIMU',
               'CACL','GOIS','RORI','JEFF','KEMO','BOHA','MISS','BISC')


TestUnits <- c('GOGA','SAMO','CABR','SAGU','PETR','TIMU',
               'KEMO','BOHA','MISS','BISC')
               
NCPNunits <- xPNR$UnitCode[xPNR$NetworkCode=='NCPN']




L1 <- AutoFigure_nlcd('COLM',
                Metric='lac1',
                AOA=c('Park','3km','30km'),
                Year=2011,
                dsn=NULL,
                Display=TRUE,
                Files=NULL)


L2 <- AutoFigure_nlcd('COLM',
                Metric='lac2',
                AOA=c('Park','3km','30km'),
                Year=2011,
                dsn=NULL,
                Display=TRUE,
                Files=NULL)


Lnc <- AutoFigure_nlcd('COLM',
                Metric='lnc',
                AOA=c('Park','3km','30km'),
                Year=2011,
                dsn=NULL,
                Display=TRUE,
                Files=NULL)

# multiple units

L1u <- AutoFigure_nlcd(NCPNunits,
                Metric='lac1',
                AOA=c('Park','3km','30km'),
                Year=2011,
                dsn=NULL,
                Display=TRUE,
                Files=NULL)


L2u <- AutoFigure_nlcd(NCPNunits,
                Metric='lac2',
                AOA=c('Park','3km','30km'),
                Year=2011,
                dsn=NULL,
                Display=TRUE,
                Files=NULL)


Lncu <- AutoFigure_nlcd(NCPNunits,
                Metric='lnc',
                AOA=c('Park','3km','30km'),
                Year=2011,
                dsn=NULL,
                Display=TRUE,
                Files=NULL)



# Multiple Years

L1y <- AutoFigure_nlcd('COLM',
                Metric='lac1',
                AOA=c('Park','3km','30km'),
                Year=c(2001,2006,2011),
                dsn=NULL,
                Display=TRUE,
                Files=NULL)


L1uy <- AutoFigure_nlcd(NCPNunits[1:3],
                Metric='lac1',
                AOA=c('Park','3km','30km'),
                Year=c(2001,2006,2011),
                dsn=NULL,
                Display=TRUE,
                Files=NULL)







