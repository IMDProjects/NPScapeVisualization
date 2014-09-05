setWindowTitle('NPScape Database  PDD Figures')
setwd('f:/NPScape/AutoFigures')
pkgList <- c("RODBC","lattice","latticeExtra",'beepr')
inst <- pkgList %in% installed.packages()
if (length(pkgList[!inst]) > 0) install.packages(pkgList[!inst])
lapply(pkgList, library, character.only = TRUE)
# library(beep)  # dinger for my use


NPScapeConnectString <- paste("Driver=SQL Server; ",
                              "Server=INP2300FCSWUPA5\\GISDATA; ",
                              "uid=NPScape_Viewer;pwd=viewer2013…;",
                              sep='')
odbcNPScape <- odbcDriverConnect(NPScapeConnectString)

source('fetch_pdd.R')

TestUnits <- c('GOGA','SAMO','CABR','SAGU','PETR','TIMU',
               'KEMO','BOHA','MISS','BISC')

PopData <- fetch_pdd(Unit=TestUnits,AOA=c('3km','30km'))
PopData$UnitCode <- factor(PopData$AOA_NAME,levels=TestUnits,ordered=TRUE)
PopData$AOA <- factor(paste(PopData$AOA,'radius'),levels=c('3km radius','30km radius'),ordered=TRUE)
useOuterStrips(barchart(AWPD_AOA~UnitCode|AOA+Census_Year,data=PopData,
                        main='Surrounding Landscape Population Density',
                        ylab='Population Density per km^2',
                        xlab='Park',
                        scales=list(x=list(alternating=1),
                                    y=list(alternating=3))
                       )    

)
