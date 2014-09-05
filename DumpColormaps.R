CMlist <- TableList[grepl('Colormap',TableList)]
for (CMi in CMlist) {
    assign(CMi,sqlFetch(odbcNPScape,CMi,as.is=TRUE,stringsAsFactors=FALSE))
} # for CMi
save(list=CMlist,file='NPScapeColormaps.Rdata')
load('d:/NPScape/NPScapeColormaps.Rdata')
