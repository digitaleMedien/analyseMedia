# Info: This Rcode gives structur and functions to analise digital media
# for example purposes, there are only blog entries at faz.net included

# Load package
library(AnalyseMedia)

# load digital media configuration
mediaResources <- read.csv2("./package/resources/digitalMedia.cnf",quote = "\"",stringsAsFactors = F)
mediaNodes     <- read.csv2("./package/resources/digitalMediaNodes.cnf",quote = "\"",stringsAsFactors = F)

# -------------------------------------------------------------------------
# Part 1: Creation of Serchterm-List via DBpedoa
# -------------------------------------------------------------------------
# Description:
# This part query the online database dbpedia for entries under a given
# concept and optional subject

# create list of search terms via dbpedia ---------------------------------
indexKeywords <- getConceptData(concept = "index",subjectLabel = "Index")
indexKeywords <- cleanCharacters(indexKeywords) # remove wrong characters
indexKeywords <- filterResultSet(indexKeywords) # remove wrong results

# export data to file
write.csv2(indexKeywords, file = "./data/indexKeywords.csv")


# -------------------------------------------------------------------------
# Part 2: Get digital articles
# -------------------------------------------------------------------------
# Description:
# This part loads all articles from a specific digital media for a given keywords
# First all urls from the media search engine are collected
# then all urls are beeing loaded

# what media should by queried?
mediaTarget <- "fazBlog" # fazBlog spiegelOnline sueddeutsche

# get articles matching keyword list ---------------------------------
# keyword from list above
keywordList <- indexKeywords$subject_label

# example keyword -> overwrites list above with static value
keywordList <- "Android" # "Korruptionsindex"

## query  page --
# save resultpages from search engine
sapply(keywordList,function(x) getResultPages(mediaTarget,x))
# extract article-urls and article attributes from query result pages
loadedResultPages <- list.files("./data/resultpages/")[grep(paste0(mediaTarget,".*",keywordList),list.files("./data/resultpages/"))]

dat1 <- lapply(loadedResultPages,function(x)  analysePage(mediaTarget,x)) %>% 
                                              do.call(rbind,.) %>% 
                                              mutate_each(funs(as.character)) %>%
                                              mutate(articleId = as.character(1:nrow(.)))

## article page --
# get the full article-pages with the url provided in resultpage
sapply(1:nrow(dat1),function(x) getPages(dat1$pageURL[x],mediaTarget,dat1$searchTerm[x],x))
# extract article attributes from article pages
loadedArticlePages <- list.files("./data/articlepages/")[grep(paste0(mediaTarget,".*",keywordList),list.files("./data/articlepages/"))]
dat2 <- lapply(loadedArticlePages,function(x) analysePage(mediaTarget,x)) %>% 
                                              do.call(rbind,.) %>% 
                                              mutate_each(funs(as.character))

## combine data and write out
dat <- left_join(dat2,dat1,by = c("articleId","searchTerm"))
# clean data
dat <- cleanData(dat,mediaTarget)
write.csv2(dat, file = paste0("./data/",mediaTarget,"_articleDatabase.csv"),row.names = FALSE,quote=1:length(dat),
           fileEncoding = "utf8")

# -------------------------------------------------------------------------
# Part 3: Get descriptive statistics for articles
# -------------------------------------------------------------------------
# Description:
# This part aggregates the article data to a set of descriptive statistics

# descriptive statistcs for articles ---------------------------------
dat <- read.csv2(paste0("./data/",mediaTarget,"_articleDatabase.csv"),
                 sep=";",stringsAsFactors = FALSE,encoding = "utf8",quote = "\"",fileEncoding = "utf8")
dat <- dat %>% 
  mutate(articleDate = as.Date(articleDate))

dat <- descrArticles(dat)
exportTable <- statTab(dat)

# frequency by time
freqDat <- freqByTime(dat)

# export summary tables for each searchterm
# ressort table
ressortTable <- lapply(unique(dat$searchTerm),function(x) aggStatTab(dat,x,"aggregatedRessortStatistic"))
sapply(seq(1:length(ressortTable)),function(x)
  write.csv2(ressortTable[[x]], 
             file = paste0("./data/table/",mediaTarget,"_aggregatedRessortStatistic_",unique(dat$searchTerm)[x],".csv"),
             row.names = FALSE
             ,quote=1:length(ressortTable[[x]]))
)

# aggregated table statistic
aggregatedExportTable <- lapply(unique(dat$searchTerm),function(x) aggStatTab(dat,x))
sapply(seq(1:length(aggregatedExportTable)),function(x)
  write.csv2(aggregatedExportTable[[x]], 
             file = paste0("./data/table/",mediaTarget,"_aggregatedExportTable_",unique(dat$searchTerm)[x],".csv"),
             row.names = FALSE
             ,quote=1:length(aggregatedExportTable[[x]]))
)


# -------------------------------------------------------------------------
# Part 3b: Create diagramms for descriptive statistics
# -------------------------------------------------------------------------

# dual plot for all searchterms in datset
sapply(unique(freqDat$searchTerm[!is.na(freqDat$searchTerm)]), function(x) dualPlot(
                                 titleMain = x,
                                 titleSub = "Subtitle",
                                 yAxisTitle = "Y-Axis",
                                 xAxisTitle = "X-Axis",
                                 refText = "Source:",
                                 freqDat,
                                 pdfName = paste0("./data/plot/",mediaTarget,"_",x,".pdf"),
                                 y2AxisTitle = "Y2-Axis",
                                 selectSearchTerm = x)
)


# -------------------------------------------------------------------------
# Part 4: Create article sampling
# -------------------------------------------------------------------------

# get article sampling ---------------------------------
# minimum 10 articles, maximum 30 articles, otherwise 5 percent
sampleDat <- sampleArticles(dat)
write.csv2(sampleDat, file = paste0("./data/sampledpages/",mediaTarget,"_articleSample.csv"),row.names = F)

# create pdf-file for sampled articels
render("./package/resources/articleSamplingDocument.Rmd",
       output_file = paste0(mediaTarget,"_articleSample.pdf"),
       output_dir = "./data/sampledpages",clean = T,envir = .GlobalEnv)
