#' remove false characters
#' 
#' @param data frame
#' @return data frame with cleaned results
#' @export
cleanCharacters <- function(.df) {
  .df %>% 
    mutate_each(funs(gsub("\"|@de|<|>","",.)))
}

#' remove false entries from results
#' 
#' @param data frame
#' @return data frame with removed invalid rows
#' @export
filterResultSet <- function(.df) {
  .df %>% 
    filter(!grepl("index[a-z]",tolower(subject_label))) %>%
    filter(!grepl("Liste",subject_label)) %>%
    filter(!grepl("Liste",concept))
}

#' no pragraph in string
#' 
#' stringVar string variable
noParagraph <- function(stringVar) {
  gsub('\u00c2',"",trimws(gsub("\\s+"," ",gsub("\n|\t|\r","",stringVar)),which = "both"))
}

#' remove html from content
#' 
removeHtml <- function(htmlContent) {
  tmp <- ""
  
  if(!is.na(htmlContent)){
    localDoc <- read_html(htmlContent)
    tmp <- html_text(html_nodes(localDoc, "p"))
  }
  
  return(tmp)
}

#' clean article data
#' 
#' @param data frame with article data
#' @param mediaTarget  name of the source media
#' @return data frame with cleane and converted results
#' @export
cleanData <- function(dat,mediaTarget) {
  
  if (mediaTarget == "fazBlog") {
    
    tmp <- dat %>% 
      mutate(articleDate = as.Date(articleDate, "%d. %B %Y")
      )
  } else if (mediaTarget %in% c("spiegelOnline","sueddeutsche")) {
    
    # set delimeter for parsing ressort
    deliRessort <- ifelse(mediaTarget=="spiegelOnline","-","\n            \n        \n            \n")
    deliOffset  <- ifelse(mediaTarget=="spiegelOnline",3,1)
    
    # setting the encoding
    tmp <- lapply(dat, function(x) {
      if(is.character(x)) {
        x <- iconv(x,from="utf8",to="utf8")
      }
      return(x)
    }) %>% 
      data.frame 
    
    tmp <- tmp %>%
      mutate_each(funs(as.character)) %>% 
      mutate(articleDate = as.Date(articleDate, "%Y-%m-%d"),
             pageRessort = noParagraph(substr(substr(pageTeaserInfo,str_locate(pageTeaserInfo,deliRessort)+2,
                                         str_length(pageTeaserInfo)),1,
                                  str_locate(substr(pageTeaserInfo,str_locate(pageTeaserInfo,deliRessort)+2,
                                                    str_length(pageTeaserInfo)),deliRessort)-deliOffset))
      ) %>% 
      rowwise() %>% 
      mutate(articleContent  = noParagraph(removeHtml(articleContent)),
             articleTeaser   = noParagraph(articleTeaser),
             articleHeadline = noParagraph(articleHeadline),
             pageTeaserInfo  = noParagraph(pageTeaserInfo),
             pageDate        = noParagraph(pageDate),
             pageHeadline    = noParagraph(pageHeadline),
             pageTeaser      = noParagraph(pageTeaser)
      )
    
    tmp$articleContent <- repair_encoding(tmp$articleContent, from="utf8")
    tmp$articleContent <- iconv(tmp$articleContent,from="utf8",to="utf8")
    # tmp <- lapply(tmp, function(x) {
    #   if(is.character(x)) {
    #     x <- repair_encoding(x, from="utf8")
    #     x <- iconv(x,from="utf8",to="utf8")
    #   }
    #   return(x)
    # }) %>%
    #   data.frame %>%
    #   mutate_each(funs(as.character))
  }
  return(tmp)
}
