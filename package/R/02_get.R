#' query DBpedia
#' 
#' @param concept name of the wikipedia concept where to look after corresponding subjects (string)
#' @param subjectLabel if specified, additionally include matching subjects (string)
#' @param spqlEndPoint the sparcle endpoint where the query is send to (string)
#' @return The function returns all subjects from DBpedia which associated to the given concept as data frame
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
          {?URL rdfs:label ?subject_label}
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

#' get query results
#' 
#' @param searchTerm searchTerm
#' @param media name of digital media (spiegel, faz)
#' @return The function query the search engine of the website and return all result-pages as htm on harddisk in the project-subfolder
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

#' helper function for getResultPages
#' 
#' @param searchTerm Suuchbegriff
#' @param pageCount Number of Resultpage
#' @return All readed html-pages are saved in a subfolder of the project
#' @export
getResultPagesHelper <- function(mediaName,searchTerm,pageCount) {
  
  qry <- makeSearchURL(mediaName,searchTerm,special = TRUE,pageCount)
  print("Loading:" %p% qry)
  
  doc <- read_html(qry)
  write_xml(doc,paste0("./data/resultpages/",mediaName,"_","resultpage_",gsub(" ","_",searchTerm,fixed=TRUE),"_page_",pageCount,".htm"))
}

#' get html
#' 
#' @param url URL of the Page
#' @param searchTerm searchTerm of url
#' @return The function download the webpage for a given url.
#' @export
getPages <- function(url,mediaName,searchTerm,pageCount) {
  
  if(mediaName=="spiegelOnline" & substr(url,1,1)=="/") {
    url <- "http://www.spiegel.de" %p0% url
  }
  
  print("Loading:" %p% url)
  
  doc <- read_html(url)
  write_xml(doc,paste0("./data/articlepages/",mediaName,"_","articlepage_",gsub(" ","_",searchTerm,fixed=TRUE),"_page_",pageCount,".htm"))
}

#' collect metadata from webpage
#' 
#' @param loadedResultPage name of saved website
#' @return The function extract article attributes from query result page or article page as data frame
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
