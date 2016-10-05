#' Query DBpedia.
#' 
#' @description Queries the DBpedia using a SPARQL endpoint.
#' @param concept name of the Wikipedia concept where to look after corresponding subjects.
#' @param subjectLabel if specified, additionally include matching subjects.
#' @param spqlEndPoint the sparcle endpoint where the query is send to.
#' @return The function returns all subjects from DBpedia which are associated to the given concept as a data frame.
#' @export
getConceptData <- function(concept="index",subjectLabel=NULL,spqlEndPoint="http://de.dbpedia.org/sparql") {
  filter <- switch (
    EXPR = length(subjectLabel)+1, 
    " (regex(STR(?concept), '" %p0% concept %p0% "'))",
    " (regex(STR(?concept), '" %p0% concept %p0% "') || 
          (regex(?subject_label, '." %p0% subjectLabel %p0% "','" %p0% tolower(substring(subjectLabel,1,1)) %p0% "')))"
  )
  
  qry <- "PREFIX dcterms: <http://purl.org/dc/terms/>
          SELECT DISTINCT ?URL ?wikiPageLength ?subject_label
          ?concept ?type ?comment ?abstract 
          WHERE {	?URL dcterms:subject ?concept .
          OPTIONAL {?URL <http://dbpedia.org/ontology/wikiPageLength> 
          ?wikiPageLength}    
          OPTIONAL {?URL rdf:type ?type}
          OPTIONAL {?URL rdfs:label ?subject_label}
          FILTER " %p0% filter %p0% ".
          OPTIONAL {?URL <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>
          ?type}
          OPTIONAL {?URL <http://www.w3.org/2000/01/rdf-schema#comment>
          ?comment}
          OPTIONAL {?URL <http://dbpedia.org/ontology/abstract> ?abstract}
          }
          LIMIT 10000"
  
  sparqlData <- SPARQL(url=spqlEndPoint,
                       query=qry,format="xml")
  return(sparqlData$results)
  
}

#' Get query results.
#' 
#' @description Queries the search engine of a digital media site for a specific searchterm.
#' @param mediaName Name of digital media. Currently only Spiegel-Online, FAZ.net and Welt.de are supported.
#' @param searchTerm The keyword the search engine should queried for.
#' @return The function returns all result-pages of the queried website as html on harddisk in the project-subfolder.
#' @export
getResultPages <- function(mediaName,searchTerm) {
  
  qry <- makeSearchURL(mediaName,searchTerm)
  
  print("Keyword:" %p% searchTerm)
  print("Loading:" %p% qry)
  
  doc <- read_html(qry)
  
  results <- tryCatch(
    getNodeText(doc, getMediaNodeText(mediaName,"results")),
    error=function(e) NA
  )
  print("Found:" %p% results)
  
  nPages    <- getNumPages(mediaName,getNodeText(doc,getMediaNodeText(mediaName,"nPages")))
  
  write_xml(doc,paste0("./data/resultpages/",mediaName,"_","resultpage_",gsub(" ","_",searchTerm,fixed=TRUE),"_page_1.htm"))
  
  if (nPages > 1) {
    sapply(seq(1:(nPages-1))+1,function(x) getResultPagesHelper(mediaName,searchTerm,x))
  }
}

#' get html
#' 
#' @description Get html-pages.
#' @param url URL of the Webpage.
#' @param mediaName Name of digital media. Currently only Spiegel-Online, FAZ.net and Welt.de are supported.
#' @param searchTerm The keyword for which the search engine returned the URL.
#' @param pageCount Number of resultpage from search engine.
#' @return The function will download the webpage for a given url as html on harddisk in the project-subfolder.
#' @details The name of the saved html-file will contain the name of the source media, the number of the resultpage the
#' url was found and the searchterm used for getting the url.
#' @export
getPages <- function(url,mediaName,searchTerm,pageCount) {
  
  if(mediaName=="spiegelOnline" & substr(url,1,1)=="/") {
    url <- "http://www.spiegel.de" %p0% url
  }
  
  print("Loading:" %p% url)
  
  doc <- read_html(url)
  write_xml(doc,paste0("./data/articlepages/",mediaName,"_","articlepage_",gsub(" ","_",searchTerm,fixed=TRUE),"_page_",pageCount,".htm"))
}

#' Collect metadata of webpage.
#' 
#' @description Analyse the Webpage and collect article attributes.
#' @param mediaName Name of digital media. Currently only Spiegel-Online, FAZ.net and Welt.de are supported.
#' @param loadedPage Name of the website-file.
#' @return The function extract article attributes from query result page or article page as data frame.
#' @export
analysePage <- function(mediaName="fazBlog",loadedPage) {
  
  if (length(grep("resultpage_",loadedPage))==1) {
    
    doc <- read_html(paste0("./data/resultpages/",loadedPage),encoding = "UTF-8")
    
    out <- data.frame(pageRessort=getNodeText(doc,getMediaNodeText(mediaName,"pageRessort")),
                      pageHeadline=getNodeText(doc,getMediaNodeText(mediaName,"pageHeadline")),
                      pageDate=getNodeText(doc,getMediaNodeText(mediaName,"pageDate")),
                      pageTeaser=getNodeText(doc,getMediaNodeText(mediaName,"pageTeaser")),
                      pageTeaserInfo=getNodeText(doc,getMediaNodeText(mediaName,"pageTeaserInfo")),
                      pageURL=getNodeText(doc,getMediaNodeText(mediaName,"pageURL")),
                      pageAuthor=getNodeText(doc,getMediaNodeText(mediaName,"pageAuthor")),
                      searchTerm=substr(gsub(paste0(mediaName,"_resultpage_"),"",loadedPage),1,regexpr("_page",gsub(paste0(mediaName,"_resultpage_"),"",loadedPage))-1))
  }
  
  if (length(grep("articlepage_",loadedPage))==1) {
    
    doc <- read_html(paste0("./data/articlepages/",loadedPage),encoding = "UTF-8")
    
    out <- data.frame(articleHeadline=getNodeText(doc,getMediaNodeText(mediaName,"articleHeadline")),
                      articleDate=getNodeText(doc,getMediaNodeText(mediaName,"articleDate")),
                      articleTeaser=getNodeText(doc,getMediaNodeText(mediaName,"articleTeaser")),
                      articleContent=getNodeText(doc,getMediaNodeText(mediaName,"articleContent")),
                      articleAuthor=getNodeText(doc,getMediaNodeText(mediaName,"articleAuthor")),
                      searchTerm=substr(gsub(paste0(mediaName,"_articlepage_"),"",loadedPage),1,regexpr("_page",gsub(paste0(mediaName,"_articlepage_"),"",loadedPage))-1),
                      articleId = substr(loadedPage,regexpr("_page_",loadedPage)+6,regexpr(".htm",loadedPage)-1))
  }
  return(out)
}

#' Sample articles
#' 
#' @description Samples articles
#' @param dat The object with article data, preferably the output from function \code{\link{cleanData}}
#' @param minArt Minimum of articles in sample.
#' @param maxArt Maximum of articles in sample.
#' @param percent Relative number of articles in sample.
#' @return The function returns a data frame with sampled articles.
#' @details The default sampling rules are minimum 10 articles, maximum 30 articles and otherwise 5 percent of available articles.
#' @examples 
#' \dontrun{
#' sampeArticles(dat,minArt=10,maxArt=30,percent=0.05)
#' sampeArticles(dat,minArt=10,maxArt=80,percent=0.20)
#' }
#' @export
sampleArticles <- function(dat,minArt=10,maxArt=30,percent=0.05) {
  dat[sample(nrow(dat), min(max(min(minArt,nrow(dat)),percent*nrow(dat)),maxArt)), ]
}
