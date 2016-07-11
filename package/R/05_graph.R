#'  Combination of linechart and barplot.
#'  
#' @description  Creates a plot combination of a linechart and a barplot.
#' @param titleMain The main title (on top) of the plot.
#' @param titleSub Sub-title (at bottom) of the plot.
#' @param yAxisTitle A label for the y axis.
#' @param xAxisTitle A label for the x axis.
#' @param refText Name of the source or author of the plot.
#' @param dat the The object with article data, preferably the output from function \code{\link{freqByTime}}.
#' @param pdfName Character string naming a pdf-file for plot output
#' @param y2AxisTitle A label for the second y axis.
#' @param selectSearchTerm The search-term for which to create the plot (defining the relevent rows of dat).
#' @examples 
#' \dontrun{
#' dualPlot(
#' titleMain = "The main title",
#' titleSub = "Subtitle",
#' yAxisTitle = "Y-Axis",
#' xAxisTitle = "X-Axis",
#' refText = "Source:",
#' dat,
#' pdfName = "./data/plot/out.pdf",
#' y2AxisTitle = "Y2-Axis",
#' selectSearchTerm = "keyword")
#' }
#' @details This is a function requiring a special data frame with specific columns. See also the function \code{\link{freqByTime}} which creates the required data structure. 
#' @export
dualPlot <- function (titleMain,titleSub = "",yAxisTitle = "",xAxisTitle = "",
                      refText = "",dat,pdfName,y2AxisTitle = "",selectSearchTerm ) {
  
  # datahandling
  tmpDat <- dat %>% 
    filter(searchTerm %in% c(NA,selectSearchTerm)) %>% 
    right_join(dat %>% select(timeDim) %>% distinct(timeDim),.,by = "timeDim") %>% 
    filter(timeDim >= min(timeDim[which(searchTerm==searchTerm)]) &
             timeDim <= max(timeDim[which(searchTerm==searchTerm)]))
  
  cairo_pdf(bg="grey98", pdfName,width=11.69,height=8.26)
  
  # define layout
  layout(matrix(c(1,2),ncol=1),heights=c(70,30))
  par(cex.axis=1.1,mai=c(0.75,1.5,0.25,0.5),omi=c(0.25,0.25,1,0.25), mgp=c(6,1,0),
      family="Lato Light",las=1)
  color <- rgb(68,90,111,150,maxColorValue=255)
  
  # first plot
  plot(tmpDat$freq,axes=F,type="n",xlab="",xlim=c(0,nrow(tmpDat)),ylim=c(0,1.15*max(tmpDat$freq)),xpd=T,
       ylab=yAxisTitle,cex.lab=1.6)
  axis(1,at=seq(1:length(tmpDat$timeDim))-1,label=tmpDat$timeDim,col=color)
  axis(2,at=py<-pretty(tmpDat$freq),col=color,cex.lab=1.2,
       labels=format(py,big.mark=".",decimal.mark = ","))
  y<-ts(tmpDat$freq,start=0,frequency=1)
  abline(h=py[-c(1,length(py))],col="lightgrey")
  lines((y))
  shapecol1<-rgb(0,128,128,50,maxColorValue=255)
  shapecol2<-rgb(0,128,128,80,maxColorValue=255)
  points(as.numeric(row.names(tmpDat)[!tmpDat$freq==0])-1,tmpDat$freq[!tmpDat$freq==0],pch=19,col=rgb(25,25,25,200,maxColorValue=255),lwd=2)
  
  # title
  mtext(titleMain,3,line=2,adj=0,cex=2.4,family="Lato Black", outer=T)
  mtext(titleSub,3,line=-0.5,adj=0,cex=1.8,font=3, outer=T)
  mtext(xAxisTitle,1,line=3,adj=0.5,cex=1.6,font=1) 
  
  ## second plot
  py<-seq(0,max(tmpDat$numWordsContent,na.rm=T),by=250)
  fpy<-format(py,big.mark=".",decimal.mark = ",")
  y2<-ts(tmpDat$numWordsContent,start=0,frequency=1)
  plot(type="h",axes=F,xlab="",ylab=y2AxisTitle,xpd=T,
       y2,col="grey",lwd=3,cex.lab=1.6,xlim=c(0,nrow(tmpDat)))
  axis(1,at=(seq(1:length(tmpDat$timeDim))-1),label=dat$timeDim,col=color)
  axis(2,at=py,col=color,cex.lab=1.2,
       labels=format(py,big.mark=".",decimal.mark = ","))
  abline(h=py,col=par("bg"),lwd=3)  
  
  # title
  mtext(refText,1,line=3,adj=1.0,cex=0.95,font=3)
  dev.off()
}