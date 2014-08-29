# panelFigure_hdd
# This variant on autoFigure_hdd creates multi-panel figures with only a
# single legend.  It only produces a single kind of figure.
#
# Parameters:
#     TargetUnits:       list of UnitCodes to be included, should be 2:6
#     AOA='30km'         AOA one of c('30km','3km','Park')
#     Start=1970         starting decade
#     Stop=2030          ending decade: can be Start + 10 out to 2100
#     Figure='G.pct'     which figure to create (see autoFigure_hdd) c('km','pct','G.km','G.pct')
#                        km scales Y in km^2 pct scales Y in % of area
#                        G. groups into 6 levels, no prefix uses the 14 levels
#     dsn=NULL           dataset if already called
#     Display=TRUE       display the figure in a window

#############################################################################
panelFigure_hdd <- function(TargetUnits,
                           AOA='30km',  # in this function, only a single value
                           Start=1970, 
                           Stop=2030,
                           Figure='G.pct',  # c('km','pct','G.km','G.pct')
                           dsn=NULL,
                           Display=TRUE) { 
   # if dsn is null, use fetch_hdd(TestUnits,AOAlist,Start=1970, End=2030) {
   if (is.null(dsn)) {
       dsn <- fetch_hdd(Unit=TargetUnits,AOA=AOA,Start=Start,End=Stop) 
   } else {
       # subset dsn passed as parameter
       dsn <- dsn[dsn$UNIT_CODE%in%TargetUnits&dsn$AOA%in%AOA,]
   }
   # if more than 1 AOA given, only use first value
   if (length(AOA)>1) AOA <- AOA[1]
   
    # grab colormaps
    # bad form, but stash them in top level environment so that they can be used again without repeated call to SQL Server
    if (!exists("hdd.cm",where=parent.frame())) 
        hdd.cm <<- sqlFetch(odbcNPScape,'tlu_hdd_Colormap',as.is=TRUE)
        
    if (!exists("hdd_Grouped.cm",where=parent.frame())) 
	hdd_Grouped.cm <<- sqlFetch(odbcNPScape,'tlu_hdd_Grouped_Colormap',as.is=TRUE)

    # strip irregular double quotes	
        hdd.cm$BASE_CLASSNAME <- gsub('\"','',hdd.cm$BASE_CLASSNAME)
        hdd_Grouped.cm$CLASSNAME <- gsub('\"','',hdd_Grouped.cm$CLASSNAME)
    
    # make sure that Urban-Regional park is first
    if (hdd.cm$BASE_CLASSNAME[14]=='Urban-Regional Park') hdd.cm <- hdd.cm[c(14,1:13),]
    if (hdd_Grouped.cm$CLASSNAME[6]=='Urban-Regional Park') hdd_Grouped.cm <- hdd_Grouped.cm[c(6,1:5),]

    # Create Palettes as named vectors of hex colors
    Palette14 <- paste('#',hdd.cm$HEX,sep='')
    names(Palette14) <- hdd.cm$BASE_CLASSNAME
    Palette6 <- paste('#',hdd_Grouped.cm$HEX,sep='')
    names(Palette6) <- hdd_Grouped.cm$CLASSNAME
    
    # Make hdd classes ordered factors
    Indices <- match(dsn$VALUE_,hdd.cm$Value_ID)
    dsn$hddClass <- hdd.cm$BASE_CLASSNAME[Indices]
    dsn$hddClass <- factor(dsn$hddClass,levels=hdd.cm$BASE_CLASSNAME,ordered=TRUE)
    dsn$hddGroupedClass <- hdd.cm$CLASSNAME[Indices]
    dsn$hddGroupedClass <- factor(dsn$hddGroupedClass,levels=hdd_Grouped.cm$CLASSNAME,ordered=TRUE)
    
    
    # full categories or grouped categories?
    if (toupper(substr(Figure,1,1)=='G')) {
        # Grouped, so dsn2
        skeleton2 <- expand.grid(unique(dsn$AOA_NAME),unique(dsn$DECADE),unique(dsn$hddGroupedClass))
        names(skeleton2) <- c('AOA_NAME','DECADE','hddGroupedClass')
        skeleton2$AREA_SQKM <- 0
        skeleton2$hddGroupedClass <- factor(skeleton2$hddGroupedClass,levels=hdd_Grouped.cm$CLASSNAME,ordered=TRUE)
        skeleton2$AOA_NAME <- as.character(skeleton2$AOA_NAME)
        skeleton2$DECADE <- as.character(skeleton2$DECADE)
        dsn2 <- aggregate(AREA_SQKM~AOA_NAME+DECADE+hddGroupedClass,
                          data=rbind(dsn[,c("AOA_NAME","DECADE","hddGroupedClass","AREA_SQKM")],skeleton2),
                          FUN=sum)
        area2 <- aggregate(AREA_SQKM~AOA_NAME+DECADE,data=dsn2,FUN=sum)
        names(area2)[3] <- 'TotalArea.sqkm'
        dsn2 <- merge(dsn2,area2,by=c('AOA_NAME','DECADE'),all.x=TRUE)
        dsn2$pct_Area <- dsn2$AREA_SQKM / dsn2$TotalArea.sqkm * 100
        dsn2 <- dsn2[order(dsn2$AOA_NAME,dsn2$DECADE,dsn2$hddGroupedClass),]      
        dsn2$UnitCode <- substr(dsn2$AOA_NAME,1,4)
        dsn2$FullName <- factor(luUnits$FULL_NAME[match(dsn2$UnitCode,luUnits$UNIT_CODE)])  
    } else {
        # not grouped, so dsn1
        # fill in zero classes
        skeleton1 <- expand.grid(unique(dsn$AOA_NAME),unique(dsn$DECADE),hdd.cm$BASE_CLASSNAME)
        names(skeleton1) <- c('AOA_NAME','DECADE','hddClass')
        skeleton1$AREA_SQKM <- 0
        skeleton1$hddClass <- factor(skeleton1$hddClass,levels=hdd.cm$BASE_CLASSNAME,ordered=TRUE)
        skeleton1$AOA_NAME <- as.character(skeleton1$AOA_NAME)
        skeleton1$DECADE <- as.character(skeleton1$DECADE)
    
        dsn1 <- aggregate(AREA_SQKM~AOA_NAME+DECADE+hddClass,
                          data=rbind(dsn[,c("AOA_NAME","DECADE","hddClass","AREA_SQKM")],skeleton1),
                          FUN=sum)
        area1 <- aggregate(AREA_SQKM~AOA_NAME+DECADE,data=dsn1,FUN=sum)
        names(area1)[3] <- 'TotalArea.sqkm'
        dsn1 <- merge(dsn1,area1,by=c('AOA_NAME','DECADE'),all.x=TRUE)
        dsn1$pct_Area <- dsn1$AREA_SQKM / dsn1$TotalArea.sqkm * 100
        dsn1 <- dsn1[order(dsn1$AOA_NAME,dsn1$DECADE,dsn1$hddClass),]  
        dsn1$UnitCode <- substr(dsn1$AOA_NAME,1,4)
        dsn1$FullName <- factor(luUnits$FULL_NAME[match(dsn1$UnitCode,luUnits$UNIT_CODE)])  
    } # bottom if grouped
    
 
    if (Figure=='km') {
        Fig  <- ggplot(dsn1,
                     aes(x=as.numeric(DECADE), y=AREA_SQKM, fill=hddClass, order=hddClass)) + 
                     geom_area(position = 'stack') +
                     scale_fill_manual(values=Palette14,
                                       guide=guide_legend(title="Development Class",reverse=TRUE)) +
                     geom_area(position = 'stack', colour="black", size=0.15,show_guide=FALSE) + 
                     theme_minimal() +
                     theme(legend.key=element_rect(colour="black",linetype="solid")) +
                     facet_wrap(~FullName) +
                     ggtitle(paste('Housing Density within ',AOA,sep='')) +
                     ylab(expression(Area~(km^2))) +
                     xlab("Decade")
    } else if (Figure=='pct') {
        Fig <- ggplot(dsn1,
                     aes(x=as.numeric(DECADE), y=pct_Area, fill=hddClass, order=hddClass)) + 
                     geom_area(position = 'stack') +
                     scale_fill_manual(values=Palette14,
                                       guide=guide_legend(title="Development Class",reverse=TRUE)) +
                     geom_area(position = 'stack', colour="black", size=0.15,show_guide=FALSE) + 
                     theme_minimal() +
                     theme(legend.key=element_rect(colour="black",linetype="solid")) +
                     facet_wrap(~FullName) +
                     ggtitle(paste('Housing Density within ',AOA,sep='')) +
                     ylab('Percent Area') +
                     xlab("Decade")   
    } else if (Figure=='G.km') {
        Fig  <- ggplot(dsn2,
                       aes(x=as.numeric(DECADE), y=AREA_SQKM, fill=hddGroupedClass, order=hddGroupedClass)) + 
                       geom_area(position = 'stack') +
                       scale_fill_manual(values=Palette6,
                                         guide=guide_legend(title="Development Class",reverse=TRUE)) +
                       geom_area(position = 'stack', colour="black", size=0.15,show_guide=FALSE) + 
                       theme_minimal() +
                       theme(legend.key=element_rect(colour="black",linetype="solid")) +
                       facet_wrap(~FullName) +
                       ggtitle(paste('Housing Density within ',AOA,sep='')) +
                       ylab(expression(Area~(km^2))) +
                       xlab("Decade")
    } else if (Figure=='G.pct') {
        Fig <- ggplot(dsn2,
                        aes(x=as.numeric(DECADE), y=pct_Area, fill=hddGroupedClass, order=hddGroupedClass)) + 
                        geom_area(position = 'stack') +
                        scale_fill_manual(values=Palette6,
                                          guide=guide_legend(title="Development Class",reverse=TRUE)) +
                        geom_area(position = 'stack', colour="black", size=0.15,show_guide=FALSE) + 
                        theme_minimal() +
                        theme(legend.key=element_rect(colour="black",linetype="solid")) +
                        facet_wrap(~FullName) +
                        ggtitle(paste('Housing Density within ',AOA,sep='')) +
                        ylab('Percent Area') +
                        xlab("Decade")
    } # if on Figure
    
    if (Display) {
        print(Fig)
    } # if (Display)    
    return(Fig)
} # bottom function  panelFigure_hdd



# out1 <- panelFigure_hdd(TargetUnit=c('CABR','SAMO'),Display=FALSE,Files='png')
# str(out1,max.level=2)
