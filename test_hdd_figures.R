setWindowTitle('NPScape Database  PDD Figures')
setwd('f:/NPScape/AutoFigures')
pkgList <- c("RODBC","grid","ggplot2",'beepr')
inst <- pkgList %in% installed.packages()
if (length(pkgList[!inst]) > 0) install.packages(pkgList[!inst])
lapply(pkgList, library, character.only = TRUE)

# note that correct values for uid and pwd need to be inserted for ***** in the line below
NPScapeConnectString <- paste("Driver=SQL Server; ",
                              "Server=INP2300FCSWUPA5\\GISDATA; ",
                              "uid=******;pwd=*********;",
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
source('fetch_hdd.R')
# source('fetch_nlcd.R')
# source('fetch_pdd.R')
source('AutoFigure_hdd.R')

TestUnits <- c('GOGA','SAMO','CABR','SAGU','PETR','AUTO','TIMU',
               'CACL','GOIS','RORI','JEFF','KEMO','BOHA','MISS','BISC')


TestUnits <- c('GOGA','SAMO','CABR','SAGU','PETR','TIMU',
               'KEMO','BOHA','MISS','BISC')

out1 <- AutoFigure_hdd(TargetUnit='CABR',Display=FALSE,Files='png')
str(out1,max.level=2)
out2 <- AutoFigure_hdd(TargetUnit=c('CABR','SAMO'),Display=TRUE,Files=NULL)
str(out2,max.level=2)
out3 <- AutoFigure_hdd(TargetUnit=TestUnits,Display=FALSE,Files=NULL)

# 4 figures for CABR & SAMO        
multiplot(out2[[1]]$km,
          out2[[2]]$km,
          out2[[1]]$G.km,
          out2[[2]]$G.km,
          cols=2)
 
# 4 figures all grouped
multiplot(out3[[1]]$G.km,
          out3[[2]]$G.km,
          out3[[3]]$G.km,
          out3[[4]]$G.km,
          cols=2)
 
 
multiplot(out3[[1]]$G.km,
           out3[[2]]$G.km,
           out3[[3]]$G.km,
           out3[[4]]$G.km,
           out3[[5]]$G.km,
           out3[[7]]$G.km,
           cols=2)
 
 multiplot(out3[[1]]$G.pct,
            out3[[2]]$G.pct,
            out3[[3]]$G.pct,
            out3[[4]]$G.pct,
            out3[[5]]$G.pct,
            out3[[7]]$G.pct,
            cols=2)
 ####################################
pf1 <- panelFigure_hdd(TargetUnits=TestUnits[1:6],
                            AOA='30km',  # in this function, only a single value
                            Start=1970, 
                            Stop=2030,
                            Figure='G.pct',  # c('km','pct','G.km','G.pct')
                            dsn=NULL,
                            Display=TRUE,
                            Files=NULL) 
                            
pf2 <- panelFigure_hdd(TargetUnits=TestUnits[7:10],
                            AOA='30km',  # in this function, only a single value
                            Start=1970, 
                            Stop=2030,
                            Figure='G.pct',  # c('km','pct','G.km','G.pct')
                            dsn=NULL,
                            Display=TRUE,
                            Files=NULL) 

