pkgname <- "AnalyseMedia"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('AnalyseMedia')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("cleanCharacters")
### * cleanCharacters

flush(stderr()); flush(stdout())

### Name: cleanCharacters
### Title: Remove false characters.
### Aliases: cleanCharacters

### ** Examples

## Not run: 
##D cleanCharacters(dat)
## End(Not run)



cleanEx()
nameEx("dualPlot")
### * dualPlot

flush(stderr()); flush(stdout())

### Name: dualPlot
### Title: Combination of linechart and barplot.
### Aliases: dualPlot

### ** Examples

## Not run: 
##D dualPlot(
##D titleMain = "The main title",
##D titleSub = "Subtitle",
##D yAxisTitle = "Y-Axis",
##D xAxisTitle = "X-Axis",
##D refText = "Source:",
##D dat,
##D pdfName = "./data/plot/out.pdf",
##D y2AxisTitle = "Y2-Axis",
##D selectSearchTerm = "keyword")
## End(Not run)



cleanEx()
nameEx("filterResultSet")
### * filterResultSet

flush(stderr()); flush(stdout())

### Name: filterResultSet
### Title: Remove false results.
### Aliases: filterResultSet

### ** Examples

## Not run: 
##D filterResultSet(dat)
## End(Not run)



cleanEx()
nameEx("freqByTime")
### * freqByTime

flush(stderr()); flush(stdout())

### Name: freqByTime
### Title: Frequency by time
### Aliases: freqByTime

### ** Examples

## Not run: 
##D freqByTime(dat,"%Y",TRUE)
##D freqByTime(dat,"%Y%m",TRUE)
## End(Not run)



cleanEx()
nameEx("removeHtml")
### * removeHtml

flush(stderr()); flush(stdout())

### Name: removeHtml
### Title: Remove html-code from content
### Aliases: removeHtml

### ** Examples

## Not run: 
##D removeHtml(htmlContent)
## End(Not run)



cleanEx()
nameEx("sampleArticles")
### * sampleArticles

flush(stderr()); flush(stdout())

### Name: sampleArticles
### Title: Sample articles
### Aliases: sampleArticles

### ** Examples

## Not run: 
##D sampeArticles(dat,minArt=10,maxArt=30,percent=0.05)
##D sampeArticles(dat,minArt=10,maxArt=80,percent=0.20)
## End(Not run)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
