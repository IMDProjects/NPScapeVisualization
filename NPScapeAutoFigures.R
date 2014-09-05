# This code hits the NPScape Access databases of summary statistics, and generates 2 versions
# of I.G. figure 2.2, on as included in the guide (Fig2), and one with the X axis showing window size and colors & symbols
# showing pattern classes (Fig2a).
#

# local directory
#setwd("F:/NPScape/AutoFigures")
setwd("j:/AutoFigures")

# required libraries
#library(RODBC)         # read from Access or ODBC datasources
#library(lattice)            # graphics
#library(latticeExtra)    # additional features for lattice
pkgList <- c("RODBC","lattice","latticeExtra")
inst <- pkgList %in% installed.packages()
if (length(pkgList[!inst]) > 0) install.packages(pkgList[!inst])
lapply(pkgList, library, character.only = TRUE)
library(beep)  # dinger for my use


##################################
#  ODBC call to NPScape SQL Server in Fort Collins
#odbcNPScape <- odbcDriverConnect("Driver=SQL Server; Server=INP2300FCSGUMO6\\GIS_Working_Data; uid=****;pwd=**********;")


NPScapeConnectString <- paste("Driver=SQL Server; ",
                              "Server=INP2300FCSWUPA5\\GISDATA; ",
                              "uid=****;pwd=*;",
                              sep='')
                              
#INP2300FCSWUPA5\GISDATA                              
 odbcNPScape <- odbcDriverConnect(NPScapeConnectString)
                             
#odbcNPScape <- odbcConnect("NPScape_Stats",
#                        uid="NPScape_Viewer",pwd='viewer2013…') 
# List of Tables & Views (stored queries)
##################################
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
# List of current housing density tables
hddTables <- TableList[substr(TableList,1,3)=='hdd'&nchar(TableList)<20]
# returns hdd records
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

TestUnits <- c('CABR','SAMO','JOTR','MOJA')
hddTest <- fetch_hdd(TestUnits,AOAlist)

##############################################################################
#  grab colormaps

pat_Density.cm <- sqlFetch(odbcNPScape,'tlu_Density_Colormap')
pat_GroupedDensity.cm <- sqlFetch(odbcNPScape,'tlu_Density_Grouped_Colormap')

# for now, build Colormaps for hdd

hdd_16_Colormap <- data.frame(BASE_CLASSNAME=c("Urban-Regional Park", "Private undeveloped",
                        "< 1.5 units / square km", "1.5 - 3 units / square km",
                        "4 - 6 units / square km", "7 - 12 units / square km",
                        "13 - 24 units / square km", "25 - 49 units / square km",
                        "50 - 145 units / square km", "146 - 494 units / square km",
                        "495 - 1234 units / square km", "1235 - 2470 units / square km",
                        "> 2470 units / square km", "Commercial/industrial"),
                        HEX1=c("#AAE100","#FFFFFF","#D7D79E","#D7C29E","#E0E089",
                        "#FFFF73","#EDD81A","#E6B045","#BF8C26","#895A44",
                        "#894444","#692B2B","#421A1A","#000000"),
                        HEX2=c("#AAE100","#FFFFFF","#D7D79E","#D7C29E","#E0E089",
                        "#FFFF73","#EDD81A","#E6B045","#BF8C26",
                        "#895A44","#894444","#692B2B","#421A1A",
                        "#707070"),
                        stringsAsFactors=FALSE)
hdd_16_Colormap$BASE_CLASSNAME <- factor(hdd_16_Colormap$BASE_CLASSNAME,
                                         levels=c("Urban-Regional Park", 
                                                  "Private undeveloped",
                                                  "< 1.5 units / square km", 
                                                  "1.5 - 3 units / square km",
                                                  "4 - 6 units / square km", 
                                                  "7 - 12 units / square km",
                                                  "13 - 24 units / square km", 
                                                  "25 - 49 units / square km",
                                                  "50 - 145 units / square km", 
                                                  "146 - 494 units / square km",
                                                  "495 - 1234 units / square km", 
                                                  "1235 - 2470 units / square km",
                                                  "> 2470 units / square km", 
                                                  "Commercial/industrial"),
                                         ordered=TRUE)
                        
hdd_6_Colormap <- data.frame(BASE_CLASSNAME=c("Urban-Regional Park","Rural","Exurban",
                                              "Suburban","Urban","Commercial/Industrial"),
                             HEX1=c('#AAE100','#FCD658', '#E88E10', '#BD6519','#894444','#000000'),
                             HEX2=c('#AAE100','#FCD658', '#E88E10', '#BD6519','#894444','#707070'),
                             stringsAsFactors=FALSE)
 hdd_6_Colormap$BASE_CLASSNAME <- factor(hdd_6_Colormap$BASE_CLASSNAME,
                                         levels=c("Urban-Regional Park","Rural","Exurban",
                                                  "Suburban","Urban","Commercial/Industrial"),
                                         ordered=TRUE)
                            
hdd.cm <- hdd_16_Colormap
hdd_Grouped.cm <- hdd_6_Colormap
                        


##############################################################################


##############################################################################


##############################################################################




# current nlcd are nchar <30
nlcdAtt <- c('lac1','lac2','lnc')
nlcdYear <- c(2001,2006,2011)
nlcd2001v11_lac1_stats_30km
nlcd2001v11_lac2_stats_30km
nlcd2001v11_lnc_stats_30km
nlcd2006v11_lac1_stats_30km
nlcd2011_LAC1_stats_30km

# tables we want
# tlu_IM_Units   
# tlu_LNC_Colormap 
# tlu_NLCD2006_LAC1_Colormap
# tlu_NLCD2006_LAC2_Colormap
# tlu_NPS_Units
# tlu_NPScape_Units
# tlu_Parks_Lower48_UpstreamWatersheds
# xref_NPS_CEC_Ecoregions 
# xref_NPS_FWS_LCC
# xref_Parks_Networks 
# xref_Parks_NPS_Regions  

WS <- sqlFetch(odbcNPScape,'tlu_Parks_Lower48_UpstreamWatersheds',
                         as.is=TRUE,stringsAsFactors=FALSE)
str(WS)


tlu_Density_Colormap <- sqlFetch(odbcNPScape,'tlu_Density_Colormap')
str(tlu_Density_Colormap)



#-- All Park-level landcover stats:
q1 <- paste("SELECT * ",
                   "  FROM nlcd2006_park_landcover ",
                   "  ORDER BY UNIT_CODE, METRIC, VALUE_ID;",sep="")
 allpark <- sqlQuery(odbcNPScape,q1)
head(allpark)
# subset variables
allpark <- allpark[,2:9]
 
 # 30km local landscapes
park30 <- sqlFetch(odbcNPScape,"nlcd2006v06_lac_stats_30km")
str(park30)
temp <- as.character(park30$AOA_NAME)
park30$UNIT_CODE <- substr(temp,1,4)
park30$AOA <- substr(temp,6,nchar(temp))

with(park30,table(CLASSNAME))
with(park30,table(AOA_NAME))




# Glue on NPS regions
head(UnitInfo)



nlcd2006.wide <- merge(nlcd2006.wide,UnitInfo[,c(1,7:10)],by.x="UNIT_CODE",by.y="UnitCode",
                                        in.x=TRUE)
    
 nlcd.class <- "Forest"                                              

xyplot(PCT_AREA.30km~PCT_AREA.Park,groups=Region,
            data=nlcd2006.wide,
            subset=nlcd2006.wide$CLASSNAME==nlcd.class,
           drop.unused.levels=TRUE,
           auto.key=list(space="bottom",columns=3,drop.unused.levels=TRUE),
           main=paste("Percent ",nlcd.class," 2006 NLCD",
           xlab="Percent Park Area",
           ylab="Percent Surrounding 30km Area")
  
 
 xyplot(PCTNatural30km~Total30km,groups=Region[,drop=TRUE],data=NLCD2006_lnc,
           subset=!UnitCode%in%c("APPA","NATR","BLRI"),
            auto.key=list(space="bottom",columns=3,drop.unused.levels=TRUE),
            main="Percent Natural Area in 30km AOA",
            xlab="30km AOA Area in km^2",
            ylab="Percent Natural")
  
 





 
 #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# -- All 30km landcover stats for parks in the Southern Rockies CEC:
q2 <- paste("SELECT UNIT_CODE, AOA, METRIC, AOA_AREA_SQKM, AREA_SQKM, PCT_AREA ",
                    "FROM [viewer].[nlcd2006_30km_landcover] ",
                    "WHERE [viewer].[nlcd2006_30km_landcover].UNIT_CODE ",
                    "IN (select UNIT_CODE FROM [dbo].[xref_NPS_CEC_Ecoregions] where NAME = 'Southern Rockies') ",
                    " ORDER BY 1; ",sep="")
  
SouthernRockies <- sqlQuery(odbcNPScape,q2)














#@@@@@@@@@@@@@@@@@



names(NLCD2006_lnc.park)
names(NLCD2006_lnc.30km)


odbcNLCD  <- odbcConnectAccess("NLCD2006_Statistics_Final.mdb")
# sql query version:
q1 <-paste( "SELECT AOA_NAME, TAREA_SQKM, AREA_SQKM, PCT_AREA ", 
                   "FROM nlcd2006_lnc_stats_Park ",
                   "WHERE nlcd2006_lnc_stats_Park.CLASSNAME='Natural'; ",sep="")
NLCD2006_lnc.park <- sqlQuery(odbcNLCD,q1)

                   
                   
                   

NLCD2006_lnc <- merge(NLCD2006_lnc.park[NLCD2006_lnc.park$CLASSNAME=="Natural",c(10,7:9)],
                                         NLCD2006_lnc.30km[NLCD2006_lnc.30km$CLASSNAME=="Natural",c(10,7:9)],
                                         by="UnitCode")
NLCD2006_lnc <- merge(NLCD2006_lnc,UnitInfo[,c(1,7,9)],by="UnitCode",in.X=TRUE)

table(NLCD2006_lnc$Region)


names(NLCD2006_lnc) <- c("UnitCode","TotalPark","NaturalPark","PCTNaturalPark",
                                               "Total30km","Natural30km","PCTNatural30km",
                                               "Network","Region")




#@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# define connection to datasource
odbcForDen <- odbcConnectAccess("NLCD2001_ForestDensity_Statistics_Final.mdb")
# grab the table of forest pattern metrics for 30km areas of interest
ForDen30 <- sqlFetch(odbcForDen,"nlcd2001_pfden_grouped_stats_30km")
close(odbcForDen)

head(ForDen30)
# Parse out UnitCode from AOA_NAME
ForDen30$UnitCode <- substr(ForDen30$AOA_NAME,1,4)
# order class names by intensity, not alphabetically
ForDen30$CLASSNAME <- factor(ForDen30$CLASSNAME,
                  levels=c("None","Rare","Patchy","Transitional","Dominant","Interior","Intact"))
ForDen30$WS <- as.factor(ForDen30$WINDOW_SIZE)
                  
                  
#########################
# Set the color palettes for symbols & lines
# 1 for window size 2 for forest intensity
# grab superpose objects
supline1 <- trellis.par.get("superpose.line")
supline2 <- supline1

supsym1 <- trellis.par.get("superpose.symbol")
supsym2 <- supsym1

# for window size
supline1$lwd=2 # thicker lines
supline1$col <- rainbow(7) # even though only need 5 colors
supsym1$col <- rainbow(7) # even though only need 5 colors

# for forest density
supline2$lwd=2 # thicker lines
supline2$col <- rev(terrain.colors(9))[3:9]  # reverse order so dark green is last, skip 2 lightest
supsym2$col <- rev(terrain.colors(9))[3:9] # reverse order so dark green is last, skip 2 lightest

    
 ################################
 
                  
 # Function to generate Figure 2, including placement of the legend 
 Fig2 <- function(target) {
     Subset <- ForDen30[ForDen30$UnitCode==target,]
     Subset <- Subset[order(Subset$WS,Subset$CLASSNAME),]
     Uinfo <- UnitInfo[UnitInfo$UnitCode==target,]
# where to put the legend
LeftMax <- max(Subset$PCT_AREA[Subset$CLASSNAME%in%c("None","Rare")])
RightMax <-  max(Subset$PCT_AREA[Subset$CLASSNAME%in%c("Interior","Intact")])
if (LeftMax<=RightMax)  LegendLocation <- c(.1,.9) else  LegendLocation <- c(.9,.9)

     Fig2 <- xyplot(PCT_AREA~CLASSNAME,groups=WS,data=Subset,
                       main=paste("Forest Density at Varying Window Sizes\n",Uinfo$UnitName,Uinfo$UnitDesignationName,sep=" "),
                       type="b",
                       xlab="Class",ylab="Percent of Area",
                       auto.key=list(points=TRUE,lines=TRUE,title="Window Size",
                                            cex=0.75,corner=LegendLocation))
}

# turn on window size definitions 
trellis.par.set("superpose.line",supline1)                  
trellis.par.set("superpose.symbol",supsym1)      

# run some test figures
print(Fig2("ROMO"))
print(Fig2("CABR"))
print(Fig2("GOGA"))
print(Fig2("ORPI"))
print(Fig2("GRSM"))
print(Fig2("YELL"))


 # Function to generate modified Figure 2, including placement of the legend 
 Fig2a <- function(target) {
     Subset <- ForDen30[ForDen30$UnitCode==target,]
     Subset <- Subset[order(Subset$WS,Subset$CLASSNAME),]
     Subset$WindowArea <- (30*Subset$WINDOW_SIZE)^2 / 1000000
     Uinfo <- UnitInfo[UnitInfo$UnitCode==target,]
# where to put the legend
LeftMax <- max(Subset$PCT_AREA[Subset$CLASSNAME%in%c("None","Rare")])
RightMax <-  max(Subset$PCT_AREA[Subset$CLASSNAME%in%c("Interior","Intact")])
if (LeftMax<=RightMax)  LegendLocation <- c(.1,.9) else  LegendLocation <- c(.9,.9)

     Fig2a <- xyplot(PCT_AREA~WINDOW_SIZE,groups=CLASSNAME,data=Subset,
                       main=paste("Forest Density at Varying Window Sizes\n",Uinfo$UnitName,Uinfo$UnitDesignationName,sep=" "),
                       type="b",
                       xlab="Window Diameter in 30m Pixels",ylab="Percent of Area",
                       auto.key=list(points=TRUE,lines=TRUE,title="Pattern Class",cex=0.75,
                                            space="right",columns=1))
     Fig2a <- xyplot(PCT_AREA~WindowArea,groups=CLASSNAME,data=Subset,
			main=paste("Forest Density at Varying Window Sizes\n",Uinfo$UnitName,Uinfo$UnitDesignationName,sep=" "),
			type="b",
			xlab="Window Area in km^2",ylab="Percent of Area",
			scales=list(x = list(log = 10)),
			auto.key=list(points=TRUE,lines=TRUE,title="Pattern Class",cex=0.75,
                        space="right",columns=1))
}


# turn on forest patch pattern definitions 
trellis.par.set("superpose.line",supline2)                  
trellis.par.set("superpose.symbol",supsym2)                  

print(Fig2a("ROMO"))
print(Fig2a("CABR"))
print(Fig2a("GOGA"))
print(Fig2a("ORPI"))
print(Fig2a("GRSM"))
print(Fig2a("YELL"))




#############################################################################
AutoFigure_hdd <- function(TargetUnit,AOA='30km',lump=FALSE,Start=1970,Stop=2030,dsn=NULL) {
   # if dsn is null, use fetch_hdd(TestUnits,AOAlist,Start=1970, End=2030) {
   if (is.null(dsn)) {
       dsn <- fetch_hdd(TargetUnit,AOA,Start=Start,End=Stop) 
   } else {
       # subset dsn passed as parameter
       dsn <- dsn[dsn$UNIT_CODE%in%TargetUnit&dsn$AOA%in%AOA,]
   }

    # grab colormaps
    # bad form, but stash them in top level environment so that they can be used again without repeated call to SQL Server
    if (!exists("hdd.cm")) hdd.cm <<- sqlFetch(odbcNPScape,'tlu_hdd_Colormap')
    if (!exists("hdd_Grouped.cm")) hdd_Grouped.cm <<- sqlFetch(odbcNPScape,'tlu_hdd_Grouped_Colormap')

    # Grab full unit names
    FullName <- luUnits$FULL_NAME[match(TargetUnit,luUnits$UNIT_CODE)]
    
    # Generate 4 Graphs per target unit and AOA:
    #         14 classes and 6 groups
    #          Area in sq km and Percent Area

    

    F1 <- ggplot(Ftab,
               aes(x=DECADE, y=AREA_SQKM, fill=hddClass,order=hddClass)) + 
               ggtitle(paste("Housing Density",UnitName,sep="\n")) +
               ylab(expression(Area~(km^2))) +
               xlab("Decade") +
               geom_area(position = 'stack') +
               scale_fill_manual(values=Palette14a,
                                           guide=guide_legend(title="Development Class",reverse=TRUE)) +
               geom_area(position = 'stack', colour="black", size=0.25,show_guide=FALSE) + 
               theme_minimal() +
               theme(legend.key=element_rect(colour="black",linetype="solid")) 
                              
               
    F2 <- ggplot(Ftab,
               aes(x=DECADE, y=PCT_AREA, fill=hddClass,order=hddClass)) + 
               ggtitle(paste("Housing Density",UnitName,sep="\n")) +
               ylab("Percent Area") +
               xlab("Decade") +
               geom_area(position = 'stack') +
               scale_fill_manual(values=Palette14a,
                                           guide=guide_legend(title="Development Class",reverse=TRUE)) +
                geom_area(position = 'stack', colour="black", size=0.25,show_guide=FALSE) + 
               theme_minimal() +
               theme(legend.key=element_rect(colour="black",linetype="solid")) 
    F3 <- ggplot(FtabSubset,
               aes(x=DECADE, y=AREA_SQKM, fill=hddClass,order=hddClass)) + 
               ggtitle(paste("Housing Density",UnitName,sep="\n")) +
               ylab(expression(Area~(km^2))) +
               xlab("Decade") +
               geom_area(position = 'stack') +
               scale_fill_manual(values=Palette14a,
                                           guide=guide_legend(title="Development Class",reverse=TRUE)) +
               geom_area(position = 'stack', colour="black", size=0.25,show_guide=FALSE) + 
               theme_minimal() +
               theme(legend.key=element_rect(colour="black",linetype="solid")) 
     F4 <- ggplot(FtabSubset,
               aes(x=DECADE, y=PCT_AREA, fill=hddClass,order=hddClass)) + 
               ggtitle(paste("Housing Density",UnitName,sep="\n")) +
               ylab("Percent Area") +
               xlab("Decade") +
               geom_area(position = 'stack') +
               scale_fill_manual(values=Palette14a,
                                           guide=guide_legend(title="Development Class",reverse=TRUE)) +
               geom_area(position = 'stack', colour="black", size=0.25,show_guide=FALSE) + 
               theme_minimal() +
               theme(legend.key=element_rect(colour="black",linetype="solid")) 
#####################
    # Grouped Categories
    G1 <- ggplot(Gtab,
               aes(x=DECADE, y=AREA_SQKM, fill=hddClass,order=hddClass)) + 
               ggtitle(paste("Housing Density",UnitName,sep="\n")) +
               ylab(expression(Area~(km^2))) +
               xlab("Decade") +
               geom_area(position = 'stack') +
               scale_fill_manual(values=Palette6a,
                                           guide=guide_legend(title="Development Class",reverse=TRUE)) +
               geom_area(position = 'stack', colour="black", size=0.25,show_guide=FALSE) + 
               theme_minimal() +
               theme(legend.key=element_rect(colour="black",size=1,linetype="solid")) 
    G2 <- ggplot(Gtab,
               aes(x=DECADE, y=PCT_AREA, fill=hddClass,order=hddClass)) + 
               ggtitle(paste("Housing Density",UnitName,sep="\n")) +
               ylab("Percent Area") +
               xlab("Decade") +
               geom_area(position = 'stack') +
               scale_fill_manual(values=Palette6a,
                                           guide=guide_legend(title="Development Class",reverse=TRUE)) +
               geom_area(position = 'stack', colour="black", size=0.25,show_guide=FALSE) + 
               theme_minimal() +
               theme(legend.key=element_rect(colour="black",size=1,linetype="solid")) 
    G3 <- ggplot(GtabSubset,
               aes(x=DECADE, y=AREA_SQKM, fill=hddClass,order=hddClass)) + 
               ggtitle(paste("Housing Density",UnitName,sep="\n")) +
               ylab(expression(Area~(km^2))) +
               xlab("Decade") +
               geom_area(position = 'stack') +
               scale_fill_manual(values=Palette6a,
                                   guide=guide_legend(title="Development Class",reverse=TRUE)) +
               geom_area(position = 'stack', colour="black", size=0.25,show_guide=FALSE) + 
               theme_minimal() +
               theme(legend.key=element_rect(colour="black",size=1,linetype="solid")) 
    G4 <- ggplot(GtabSubset,
              aes(x=DECADE, y=PCT_AREA, fill=hddClass,order=hddClass)) + 
              ggtitle(paste("Housing Density",UnitName,sep="\n")) +
               ylab("Percent Area") +
              xlab("Decade") +
              geom_area(position = 'stack') +
              scale_fill_manual(values=Palette6a,
                                          guide=guide_legend(title="Development Class",reverse=TRUE)) +
               geom_area(position = 'stack', colour="black", size=0.25,show_guide=FALSE) + 
               theme_minimal() +
               theme(legend.key=element_rect(colour="black",size=1,linetype="solid")) 

    ###############################
    # dump graphs to files
    Plots <- c("F1","F2","F3","F4","G1","G2","G3","G4")
    GraphNames <- c("all2100km","all2100pct",
                                  "all2050km","all2050pct",
                                  "group2100km","group2100pct",
                                  "group2050km","group2050pct")
    # write graphs to png. pdf, and wmf files
    for (G in 1:8) {
        ggsave(filename=paste(TargetUnit,"_",GraphNames[G],".png",sep=""),
                       plot=get(Plots[G]),
                      width=7,height=4,units="in")
         ggsave(filename=paste(TargetUnit,"_",GraphNames[G],".pdf",sep=""),
                       plot=get(Plots[G]),
                      width=7,height=4,units="in")             
         ggsave(filename=paste(TargetUnit,"_",GraphNames[G],".emf",sep=""),
                       plot=get(Plots[G]),
                      width=7,height=4,units="in")                            
    } # bottom loop G over graphs
} # bottom AutoFigure_hdd

