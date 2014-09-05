#  function AutoFigure_hdd
#  This function produces figures of SERGoM housing density over time
#  for one or more Units and one or more AOAs,
#  Parameters:
#    TargetUnit            a character vector of 1 or more UnitCode values
#    AOA='30km'            the neighborhood or Area Of Interest c('park','3km','30km')
#    Start=1970,           earliest decade
#    Stop=2030,            last decade  can be up to 2100
#    yUnits='AREA_SQKM'    the units of the Y axis  c('AREA_SQKM','pct_Area')
#    LabelType='Name',     c('Raw','Lumped','Name')
#                          Raw uses 14 clases, Lumped uses 6 classes with numrtiv
#                          cuts in legend, Name uses 6 with "Urban" etc., 
#    UseLegend=TRUE,       not currently used, will support figures without legends
#    dsn=NULL,             dataframe to use, if NULL, fetch_hdd will be called
#    Display=TRUE,         whether to display the figure on the current device
#    FileName=NULL         if not null, the filename for directly writing the figure to.
#                          File type guessed from filename extension
#                          c('.png','.jpg','.bmp','.pdf')  see ?ggsave
#############################################################################
# This version allows both multiple target units and multiple AOAs
# I strongly recommend only using multiple target units for a single AOA,
# and possibly multiple AOAs for a single Unit.
#  # of Unit  # of AOAs   Result
#      1         1          single figure      
#     >1         1          facet by TargetUnit
#      1        >1          facet by AOA
#     >1        >1          facet columns by AOA rows by Unit
#                               


AutoFigure_hdd <- function(TargetUnit,
                           AOA='30km',
                           Start=1970, 
                           Stop=2030,
                           yUnits='AREA_SQKM',  # c('AREA_SQKM','pct_Area')
                           LabelType='Name',   # c('Raw','Lumped','Name')
                           UseLegend=TRUE,
                           dsn=NULL,
                           Display=TRUE,
                           Files=NULL) {
   # if dsn is null, use fetch_hdd(TestUnits,AOAlist,Start=1970, End=2030) {
   if (is.null(dsn)) {
       FetchedHDD <- fetch_hdd(Unit=TargetUnit,AOA=AOA,Start=Start,End=Stop)
       dsn <- FetchedHDD$Result
   } else {
       # subset dsn passed as parameter
       dsn <- dsn[dsn$UNIT_CODE%in%TargetUnit&dsn$AOA%in%AOA,]
   }

#######################################
    # grab colormaps
    # bad form, but stash them in top level environment so that they can be used again without repeated call to SQL Server
    # always need 14 class colormap
    if (!exists("hdd.cm",where=parent.frame())) 
            hdd.cm <- sqlFetch(odbcNPScape,'tlu_hdd_Colormap',as.is=TRUE)
    # strip irregular double quotes	
    hdd.cm$BASE_CLASSNAME <- gsub('\"','',hdd.cm$BASE_CLASSNAME)
    
    # Choose which classification to use, 14 or 6 groups
    if (tolower(LabelType)=='raw') {
        # leave as 14 classes
        ColorMap <- hdd.cm    
        # make sure that Urban-Regional park is first
        if (ColorMap$CLASSNAME[14]=='Urban-Regional Park') ColorMap <- ColorMap[c(14,1:13),]
        ColorMap$CLASS <- ColorMap$BASE_CLASSNAME
        dsn$CLASS <- ColorMap$CLASS[match(dsn$VALUE_,ColorMap$Value_ID)]
    } else {
        # eiher 'Lumped' or 'Name'
        if (!exists("hdd_Grouped.cm",where=parent.frame())) 
            hdd_Grouped.cm <- sqlFetch(odbcNPScape,'tlu_hdd_Grouped_Colormap',as.is=TRUE)
        # strip irregular double quotes	
        hdd_Grouped.cm$BASE_CLASSNAME <- gsub('\"','',hdd_Grouped.cm$BASE_CLASSNAME)
        ColorMap <- hdd_Grouped.cm    
        # make sure that Urban-Regional park is first
        if (ColorMap$CLASSNAME[6]=='Urban-Regional Park') ColorMap <- ColorMap[c(6,1:5),]
        # create crosswalk to map to grouped classes
        Lumper <- merge(hdd.cm[,c(2,6,8)],hdd_Grouped.cm[,c(6,8)],by='CLASSNAME')
        names(Lumper) <- c('CLASSNAME','Value_ID','BASE_CLASSNAME.14','BASE_CLASSNAME.6')
        Lumper <- Lumper[order(Lumper$Value_ID),c(2,1,3,4)]
        if (tolower(LabelType)=='name')  {
            ColorMap$CLASS <- ColorMap$CLASSNAME 
            dsn$CLASS <- Lumper$CLASSNAME[match(dsn$VALUE_,Lumper$Value_ID)]
        } else {
            ColorMap$CLASS <- ColorMap$BASE_CLASSNAME    
            dsn$CLASS <- Lumper$BASE_CLASSNAME.6[match(dsn$VALUE_,Lumper$Value_ID)]
        } # ifelse LabelType          
    } # ifelse LabelType=='Raw'
     
    ColorMap$CLASS <- factor(ColorMap$CLASS,levels=ColorMap$CLASS,ordered=TRUE) 
    # Create Palettes as named vectors of hex colors
    Palette <- paste('#',ColorMap$HEX,sep='')
    names(Palette) <- ColorMap$CLASS
    
    dsn$CLASS <- factor(dsn$CLASS,levels=ColorMap$CLASS,ordered=TRUE) 

    
    # fill in zero classes
    skeleton <- expand.grid(unique(dsn$AOA_NAME),unique(dsn$DECADE),ColorMap$CLASS)
    names(skeleton) <- c('AOA_NAME','DECADE','CLASS')
    skeleton$AREA_SQKM <- 0
    skeleton$CLASS <- factor(skeleton$CLASS,levels=ColorMap$CLASS,ordered=TRUE)
    skeleton$AOA_NAME <- as.character(skeleton$AOA_NAME)
    skeleton$DECADE <- as.character(skeleton$DECADE)
    
    dsn1 <- aggregate(AREA_SQKM~AOA_NAME+DECADE+CLASS,
                      data=rbind(dsn[,c("AOA_NAME","DECADE","CLASS","AREA_SQKM")],skeleton),
                      FUN=sum)
                      
    area1 <- aggregate(AREA_SQKM~AOA_NAME+DECADE,data=dsn1,FUN=sum)
    names(area1)[3] <- 'TotalArea.sqkm'
    dsn1 <- merge(dsn1,area1,by=c('AOA_NAME','DECADE'),all.x=TRUE)
    dsn1$pct_Area <- dsn1$AREA_SQKM / dsn1$TotalArea.sqkm * 100
    dsn1$UnitCode <- substr(dsn1$AOA_NAME,1,4)
    dsn1$AOA <- factor(substr(dsn1$AOA_NAME,6,nchar(dsn1$AOA_NAME)),
                       levels=c('Park','3km','30km'),ordered=TRUE)
     
    dsn1$FullName <- factor(luUnits$FULL_NAME[match(dsn1$UnitCode,luUnits$UNIT_CODE)])  
    dsn1$Year <- as.numeric(dsn1$DECADE)
    dsn1 <- dsn1[order(dsn1$UnitCode,dsn1$AOA,dsn1$Year,dsn1$CLASS),]
    # pick units for Y axes
    if (grepl('km',yUnits,ignore.case=TRUE)) {
        dsn1$Y <- dsn1$AREA_SQKM
        ylab <- expression(Area~(km^2))
    } else {
        dsn1$Y <- dsn1$pct_Area
        ylab <- 'Percent Area'
    }  # bottom pick units for Y
    

    #  # of Unit  # of AOAs   Result
    #      1         1          single figure      
    #     >1         1          facet by TargetUnit
    #      1        >1          facet by AOA
    #     >1        >1          facet columns by AOA rows by Unit
    if (length(TargetUnit)==1) {
        # only 1 UnitCode
        if (length(AOA)==1) {
            # Single AOA, too, so single figure
            # cat('A\n')
            FullName <- dsn1$FullName[1]
            Fig <- ggplot(dsn1,
                          aes(x=Year, y=Y, fill=CLASS, order=CLASS)) + 
                          geom_area(position = 'stack') +
                          scale_fill_manual(values=Palette,
                                            guide=guide_legend(title="Development Class",reverse=TRUE)) +
                          geom_area(position = 'stack', colour="black", size=0.15,show_guide=FALSE) + 
                          theme_minimal() +
                          theme(legend.key=element_rect(colour="black",linetype="solid")) +
                          ggtitle(paste(FullName,'  ',dsn1$AOA[1],
                                        'Neighborhood\nSERGoM Modeled Housing Density')) +
                          ylab(ylab) +
                          xlab("Decade")
        } else {   
            # multiple AOAs   facet by AOA
            # cat('B\n')
            Fig <- ggplot(dsn1,
                          aes(x=Year, y=Y, fill=CLASS, order=CLASS)) + 
                          geom_area(position = 'stack') +
                          scale_fill_manual(values=Palette,
                                            guide=guide_legend(title="Development Class",reverse=TRUE)) +
                          geom_area(position = 'stack', colour="black", size=0.15,show_guide=FALSE) + 
                          facet_grid(AOA~.) +                           
                          theme_minimal() +
                          theme(legend.key=element_rect(colour="black",linetype="solid")) +
                          ggtitle(paste(FullName,'\nSERGoM Modeled Housing Density')) +
                          ylab(ylab) +
                          xlab("Decade")
            
        } # end ifelse length AOA        
    } else {  # multiple UnitCodes
        if (length(AOA)==1) {
            # Multiple Units, 1 AOA  facet by Unit
            # cat('C\n')
            Fig <- ggplot(dsn1,
                          aes(x=Year, y=Y, fill=CLASS, order=CLASS)) + 
                          geom_area(position = 'stack') +
                          scale_fill_manual(values=Palette,
                                            guide=guide_legend(title="Development Class",reverse=TRUE)) +
                          geom_area(position = 'stack', colour="black", size=0.15,show_guide=FALSE) + 
                          facet_grid(FullName~.) +                           
                          theme_minimal() +
                          theme(legend.key=element_rect(colour="black",linetype="solid")) +
                          ggtitle(paste(dsn1$AOA[1],
                                        'Neighborhood\nSERGoM Modeled Housing Density')) +
                          ylab(ylab) +
                          xlab("Decade")
         } else {
            # Multiple Units, Multiple AOAs  facet by both  UGLY!!
            # cat('D\n')
            FullName <- dsn1$FullName[1]
            Fig <- ggplot(dsn1,
                          aes(x=Year, y=Y, fill=CLASS, order=CLASS)) + 
                          geom_area(position = 'stack') +
                          scale_fill_manual(values=Palette,
                                            guide=guide_legend(title="Development Class",reverse=TRUE)) +
                          geom_area(position = 'stack', colour="black", size=0.15,show_guide=FALSE) + 
                          facet_wrap(FullName~AOA) +                           
                          theme_minimal() +
                          theme(legend.key=element_rect(colour="black",linetype="solid")) +
                          ggtitle('SERGoM Modeled Housing Density') +
                          ylab(ylab) +
                          xlab("Decade")
         } # end ifelse length AOA     
    }  # bottom ifelse multiple UnitCodes
    
    # display plots?
    if (Display) plot(Fig)
    if (!is.null(Files)) ggsave(filename=Files,plot=Fig)
    return(list(Fig=Fig,dsn=dsn))
} # bottom autoFigure_nlcd 

