#############################################################################
AutoFigure_hdd <- function(TargetUnit,
                           AOA='30km',
                           Start=1970, 
                           Stop=2030,
                           dsn=NULL,
                           Display=TRUE,
                           Files=NULL) {
   # if dsn is null, use fetch_hdd(TestUnits,AOAlist,Start=1970, End=2030) {
   if (is.null(dsn)) {
       dsn <- fetch_hdd(Unit=TargetUnit,AOA=AOA,Start=Start,End=Stop)$Result 
   } else {
       # subset dsn passed as parameter
       dsn <- dsn[dsn$UNIT_CODE%in%TargetUnit&dsn$AOA%in%AOA,]
   }

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

  

    Reps <- expand.grid(TargetUnit,AOA)
    names(Reps) <- c('UnitCode','AOA')
    Reps$AOA_NAME <- paste(Reps$UnitCode,Reps$AOA,sep='_')
     # Grab full unit names
    Reps$FullName <- luUnits$FULL_NAME[match(Reps$UnitCode,luUnits$UNIT_CODE)]   
    Reps$TitleText <- paste(Reps$FullName,'\nHousing Density within ',Reps$AOA,sep='')
    
    for (i in 1:nrow(Reps)) {
        cat(Reps$AOA_NAME[i],'\n')
        km <- ggplot(dsn1[dsn1$AOA_NAME==Reps$AOA_NAME[i],],
                     aes(x=as.numeric(DECADE), y=AREA_SQKM, fill=hddClass, order=hddClass)) + 
                     geom_area(position = 'stack') +
                     scale_fill_manual(values=Palette14,
                                       guide=guide_legend(title="Development Class",reverse=TRUE)) +
                     geom_area(position = 'stack', colour="black", size=0.15,show_guide=FALSE) + 
                     theme_minimal() +
                     theme(legend.key=element_rect(colour="black",linetype="solid")) +
                     ggtitle(Reps$TitleText[i]) +
                     ylab(expression(Area~(km^2))) +
                     xlab("Decade")
        pct <- ggplot(dsn1[dsn1$AOA_NAME==Reps$AOA_NAME[i],],
                     aes(x=as.numeric(DECADE), y=pct_Area, fill=hddClass, order=hddClass)) + 
                     geom_area(position = 'stack') +
                     scale_fill_manual(values=Palette14,
                                       guide=guide_legend(title="Development Class",reverse=TRUE)) +
                     geom_area(position = 'stack', colour="black", size=0.15,show_guide=FALSE) + 
                     theme_minimal() +
                     theme(legend.key=element_rect(colour="black",linetype="solid")) +
                     ggtitle(Reps$TitleText[i]) +
                     ylab('Percent Area') +
                     xlab("Decade")

        G.km <- ggplot(dsn2[dsn2$AOA_NAME==Reps$AOA_NAME[i],],
                       aes(x=as.numeric(DECADE), y=AREA_SQKM, fill=hddGroupedClass, order=hddGroupedClass)) + 
                       geom_area(position = 'stack') +
                       scale_fill_manual(values=Palette6,
                                         guide=guide_legend(title="Development Class",reverse=TRUE)) +
                       geom_area(position = 'stack', colour="black", size=0.15,show_guide=FALSE) + 
                       theme_minimal() +
                       theme(legend.key=element_rect(colour="black",linetype="solid")) +
                       ggtitle(Reps$TitleText[i]) +
                       ylab(expression(Area~(km^2))) +
                       xlab("Decade")
        G.pct <- ggplot(dsn2[dsn2$AOA_NAME==Reps$AOA_NAME[i],],
                        aes(x=as.numeric(DECADE), y=pct_Area, fill=hddGroupedClass, order=hddGroupedClass)) + 
                        geom_area(position = 'stack') +
                        scale_fill_manual(values=Palette6,
                                          guide=guide_legend(title="Development Class",reverse=TRUE)) +
                        geom_area(position = 'stack', colour="black", size=0.15,show_guide=FALSE) + 
                        theme_minimal() +
                        theme(legend.key=element_rect(colour="black",linetype="solid")) +
                        ggtitle(Reps$TitleText[i]) +
                        ylab('Percent Area') +
                        xlab("Decade")
                        
        assign(Reps$AOA_NAME[i],list(km=km,pct=pct,G.km=G.km,G.pct=G.pct))
        if (exists('OutList')) OutList <- c(OutList,list(get(Reps$AOA_NAME[i]))) else OutList <- list(get(Reps$AOA_NAME[i]))
    
        # display plots?
        if (Display) {
            oldAsk <- par('ask')
            par(ask=TRUE)
            print(km)
            print(pct)
            print(G.km)
            print(G.pct)
            par(ask=oldAsk) # be nice & restore value
        } # if (Display)    
    
        # create files?
        if (!is.null(Files)) {
            for (Fig in c('km','pct','G.km','G.pct')) {
                if (Files=='png') {
                    png(filename=paste(Reps$AOA_NAME[i],'_',Fig,'.png',sep=''),
                        width=10,height=6,units='in',res=144)
                    print(get(Fig))
                    dev.off()
                } else if (Files%in%c('jpeg','jpg')) {
                    jpeg(filename=paste(Reps$AOA_NAME[i],'_',Fig,'.jpeg',sep=''),
                         width=10,height=6,units='in',res=72,quality=100)
                    print(get(Fig))
                    dev.off()
                }
            }  # for loop over Fig
        } # !is.null(Files)
    } # for i in 1:nrow(Reps)
    print(Reps$AOA_NAME)
    names(OutList) <- Reps$AOA_NAME
    
    return(OutList)
} # bottom function  AutoFigure_hdd

# out1 <- AutoFigure_hdd(TargetUnit='CABR',Display=FALSE,Files='png')
# str(out1,max.level=2)
# out2 <- AutoFigure_hdd(TargetUnit=c('CABR','SAMO'))
# str(out2,max.level=2)
