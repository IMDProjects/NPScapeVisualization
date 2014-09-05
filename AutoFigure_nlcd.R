 #    Unit:  a vector of 1 or more unit codes
#    metric: a vector of one or more of {'lac1','lac2','lnc'}

AutoFigure_nlcd <- function(TargetUnit,
                             Metric='lac1',
                             AOA=c('Park','3km','30km'),
                             Year=2011,
                             dsn=NULL,
                             Display=TRUE,
                             Files=NULL) {
   # if dsn is null, use fetch_nlcd(TestUnits,AOAlist,Start=1970, End=2030) {
   if (is.null(dsn)) {
       FetchedNLCD <- fetch_nlcd(mdb= odbcNPScape,
                                 Unit= TargetUnit, #c('CABR','SAMO'),
                                 Metric=Metric,
                                 AOA=AOA,
                                 Year=Year)  # includes Result dataframe and SQL query string
       dsn <- FetchedNLCD$Result
   } else {
       # subset dsn passed as parameter
       dsn <- dsn[dsn$UNIT_CODE%in%TargetUnit&
                  dsn$AOA%in%AOA&
                  dsn$Year%in%Year,]
   }
   dsn <- dsn[!is.na(dsn$CLASSNAME),]
   dsn$UnitCode <- substr(dsn$AOA_NAME,1,4)
   dsn$AOA <- substr(dsn$AOA_NAME,6,nchar(dsn$AOA_NAME))
   dsn$AOA <- factor(dsn$AOA,levels=c('Park','3km','30km'),ordered=TRUE)
   
   dsn$FullName <- factor(luUnits$FULL_NAME[match(dsn$UnitCode,luUnits$UNIT_CODE)])  
   dsn$Year <- as.numeric(dsn$Image)
   dsn <- dsn[order(dsn$UnitCode,dsn$AOA,dsn$CLASSNAME),]
#   print(str(dsn))
   # grab colormaps
   # bad form, but stash them in top level environment so that they can be used again without repeated call to SQL Server
   if (!exists("NLCD_LAC1.cm",where=parent.frame())) 
       NLCD_LAC1.cm <<- sqlFetch(odbcNPScape,"tlu_NLCD_LAC1_Colormap",as.is=TRUE)
   if (!exists("NLCD_LAC2.cm",where=parent.frame())) 
       NLCD_LAC2.cm <<- sqlFetch(odbcNPScape,"tlu_NLCD_LAC2_Colormap",as.is=TRUE)
   if (!exists("LNC.cm",where=parent.frame())) 
       LNC.cm <<- sqlFetch(odbcNPScape,"tlu_LNC_Colormap",as.is=TRUE)

   # Create Palettes as named vectors of hex colors
   Palette.L1 <- paste('#',NLCD_LAC1.cm$HEX,sep='')
   names(Palette.L1) <- NLCD_LAC1.cm$CLASSNAME
   Palette.L2 <- paste('#',NLCD_LAC2.cm$HEX,sep='')
   names(Palette.L2) <- NLCD_LAC2.cm$CLASSNAME
   Palette.LNC <- paste('#',LNC.cm$HEX,sep='')
   names(Palette.LNC) <- LNC.cm$CLASSNAME

# assign right factor levels and choose palette for given Metric
   if (Metric=='lac1') {
       dsn$CLASSNAME <- factor(dsn$CLASSNAME,
                               levels=NLCD_LAC1.cm$CLASSNAME,
                               ordered=TRUE)
       Palette <- Palette.L1                        
   } else if  (Metric=='lac2') {                            
       dsn$CLASSNAME <- factor(dsn$CLASSNAME,
                               levels=NLCD_LAC2.cm$CLASSNAME,
                               ordered=TRUE)
       Palette <- Palette.L2                        
   } else {   # LNC                         
       dsn$CLASSNAME <- factor(dsn$CLASSNAME,
                               levels=LNC.cm$CLASSNAME,
                               ordered=TRUE)
       Palette <- Palette.LNC                        
   }

###############
# Graph Cases
#     single Year, single Unit gives just AOA
#     single Year, multiple Units gives facet by FullName
#     multiple Year single unit facet by Year
#     multiple Year multiple Unit facet rows by Unit columns by Year
   if (length(Year)==1&length(TargetUnit)==1) {
       #     single Year, single Unit gives just AOA
       FullName <- dsn$FullName[1]
       Fig <- ggplot(dsn,
                     aes(x=AOA, y=PCT_AREA,fill=CLASSNAME)) +
                     geom_bar(stat='identity', width=0.75, position= position_stack(width=.5)) +
                     scale_fill_manual(values=Palette,
                                       guide=guide_legend(title=paste(Metric,"Class"),reverse=TRUE)) +
                     theme_minimal() +
                     theme(legend.key=element_rect(colour="black",linetype="solid")) +
                     ggtitle(paste(FullName,'\n',Year,'NLCD', Metric, 'Land Cover')) +
                     ylab('Percent Area') +
                     xlab("Area Extent")       
   } else if (length(Year)==1&length(TargetUnit)>1){
       #     single Year, multiple Units gives facet by FullName
       Fig <- ggplot(dsn,
                     aes(x=AOA, y=PCT_AREA,fill=CLASSNAME)) +
                     geom_bar(stat='identity', width=0.75, position= position_stack(width=.5)) +
                     scale_fill_manual(values=Palette,
                                       guide=guide_legend(title=paste(Metric,"Class"),reverse=TRUE)) +
                     facet_wrap(~FullName) + 
                     theme_minimal() +
                     theme(legend.key=element_rect(colour="black",linetype="solid")) +
                     ggtitle(paste(Year,'NLCD', Metric, 'Land Cover')) +
                     ylab('Percent Area') +
                     xlab("Area Extent")
   } else if (length(Year)>1&length(TargetUnit)==1) {
       #     multiple Year single unit facet by Year
       Fig <- ggplot(dsn,
                     aes(x=AOA, y=PCT_AREA,fill=CLASSNAME)) +
                     geom_bar(stat='identity', width=0.75, position= position_stack(width=.5)) +
                     scale_fill_manual(values=Palette,
                                       guide=guide_legend(title=paste(Metric,"Class"),reverse=TRUE)) +
                     facet_wrap(~Year) + 
                     theme_minimal() +
                     theme(legend.key=element_rect(colour="black",linetype="solid")) +
                     ggtitle(paste(FullName,'NLCD', Metric, 'Land Cover')) +
                     ylab('Percent Area') +
                     xlab("Area Extent")
   } else {
       #     multiple Year multiple Unit facet rows by Unit columns by Year
       Fig <- ggplot(dsn,
                     aes(x=AOA, y=PCT_AREA,fill=CLASSNAME)) +
                     geom_bar(stat='identity', width=0.75, position= position_stack(width=.5)) +
                     scale_fill_manual(values=Palette,
                                       guide=guide_legend(title=paste(Metric,"Class"),reverse=TRUE)) +
                     facet_grid(FullName~Year) + 
                     theme_minimal() +
                     theme(legend.key=element_rect(colour="black",linetype="solid")) +
                     ggtitle(paste('NLCD', Metric, 'Land Cover')) +
                     ylab('Percent Area') +
                     xlab("Area Extent")
   } # bottom cases for faceting  
   if (Display) plot(Fig)
   if (!is.null(Files)) ggsave(filename=Files,plot=Files)
   return(list(Fig=Fig,dsn=dsn))
} # bottom autoFigure_nlcd 

