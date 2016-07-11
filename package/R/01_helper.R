#' paste functions
#' 
`%p%`        <- function(x, y) paste(x, y)
`%p0%`       <- function(x, y) paste0(x, y)

#' get xml node
#' 
getNodeText <- function(doc,xpathAttr) {
  
  localAttr   <- ifelse(!identical(xpathAttr, character(0)),xpathAttr[2],"")
  localXpath  <- ifelse(!identical(xpathAttr, character(0)),xpathAttr[1],NA)

  if (!localAttr == "") {

    tmp <- tryCatch(
      html_attr(html_nodes(doc, xpath = localXpath), localAttr), 
      error=function(e) NA)
    if(identical(tmp, character(0))) {
      tmp <- NA
    }
      
  } else  {
  
    tmp <- tryCatch(
        html_text(html_nodes(doc, xpath = localXpath)), 
        error=function(e) NA)
  if(identical(tmp, character(0))) {
    tmp <- NA
    
  }
  }
  return(tmp)
}

#' sample articles
#' 
#' @param dat data frame with article data
#' @return data frame with sampled articles (minimum 10 articles, maximum 30 articles, otherwise 5 percent)
#' @export
sampeArticles <- function(dat,minArt=10,maxArt=30,percent=0.05) {
  dat[sample(nrow(dat), min(max(min(minArt,nrow(dat)),percent*nrow(dat)),maxArt)), ]
}

#' sanitizeLatexS
#' 
sanitizeLatexS <- function(str) {
  gsub('([#$%&~_\\^\\\\{}])', '\\\\\\\\\\1', str, perl = TRUE);
}

#' begin string with letter
#' 
startStringWithLetter <- function(txt) {
  substring(txt,as.numeric(regexpr("[a-zA-Z]+",txt)))
}

#' build export table
#' 
#' @param dat data frame with article data
#' @return data frame with selected columns
#' @export
statTab <- function(.dat,type="articleStatistic"){
  if (type == "articleStatistic") {
  tmp <- .dat %>% select(searchTerm,
                  articleHeadline,
                  articleAuthor,
                  articleDate,
                  pageRessort,
                  numWordsContent,
                  numParagraph,
                  numWordsTitle,
                  numSerchTerm,
                  searchTermInTitel,
                  searchTermFirstPg,
                  serchTermFirstSeen,
                  sentenceLength
  )
  }
  return(tmp)
}

#' build aggregated export table
#' 
#' @param dat data frame with article data
#' @param type export table template
#' @return data frame with selected columns
#' @export
aggStatTab <- function(.dat,selectSearchTerm,type="aggregatedArticleStatistic"){
 
  if (type == "aggregatedArticleStatistic") {
    tmp <- .dat %>%
    filter(searchTerm == selectSearchTerm) %>% 
    select(numWordsContent,numParagraph,numWordsTitle,numSerchTerm,
           searchTermInTitel,searchTermFirstPg,serchTermFirstSeen,sentenceLength) %>% 
    summarise_each(funs(min,median,max,mean,sd)) %>% 
    gather("measure") %>% 
    separate("measure",c("measure","statFunc"),"_") %>% 
    spread("statFunc","value")
  }
  
  if (type == "aggregatedRessortStatistic") {
    tmp <- .dat %>% 
    filter(searchTerm == selectSearchTerm) %>% 
    group_by(pageRessort) %>% 
    summarise(num = n()) %>% 
    mutate(freq = num/sum(num)) %>% 
    data.frame
  }
  return(tmp)
}  

#' getNodeText from config
#' 
getMediaNodeText <- function(mediaName,nodeTarget) {
  filter(mediaNodes,media==mediaName & target==nodeTarget)[c(3,4)] %>% unlist(use.names = FALSE)
}

#' make search url
#' 
makeSearchURL <- function(mediaName,searchTerm,special = FALSE,localPageCount = NULL) {
  
  tmpQry <- filter(mediaResources,media==mediaName)$urlPreStatement %p0%
    gsub(" ","+",searchTerm,fixed=TRUE) %p0%
    filter(mediaResources,media==mediaName)$urlPostStatement
  
  if(special == TRUE & mediaName == "fazBlog") {
    tmpQry <- "http://www.faz.net/suche/s" %p0% localPageCount %p0% ".html?query=%22" %p0% 
      gsub(" ","+",searchTerm,fixed=TRUE) %p0% 
      "%22&BTyp=redaktionelleInhalte&chkBoxType_3=on&resultsPerPage=80&to=" %p0% format(Sys.time(),"%d.%m.%Y") %p0% "&from=TT.MM.JJJJ#FAZContent"
  }
  
  if(special == TRUE & mediaName == "spiegelOnline") {
    tmpQry <- tmpQry %p0% "&pageNumber=" %p0% localPageCount
  }
  
  if(special == TRUE & mediaName == "sueddeutsche") {
    tmpQry <- gsub("\\?",paste0("/page/",localPageCount,"\\?"),filter(mediaResources,media==mediaName)$urlPreStatement) %p0%
      gsub(" ","+",searchTerm,fixed=TRUE) %p0%
      filter(mediaResources,media==mediaName)$urlPostStatement
  }

  return(tmpQry)
}

#' getNumPages
#' 
getNumPages <- function(localMediaName,nodeText) {
  
  if(localMediaName == "fazBlog") {
    as.numeric(unlist(strsplit(
      nodeText
      ," "))[2])
  }

  if(localMediaName == "spiegelOnline") {
    max(as.numeric(str_extract_all(nodeText,"\\(?[0-9,.]+\\)?") %>% unlist))
  }
  
  if(localMediaName == "sueddeutsche") {
    ifelse(any(!is.na(nodeText)),
    max(as.numeric(nodeText),na.rm = T),
    1)
  }

}
