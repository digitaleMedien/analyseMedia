#' Remove false characters.
#' 
#' @description Removes special metadata characters from DBpedia-Results.
#' @param .df The data frame with results from DBpedia, preferably the output from function \code{\link{getConceptData}}.
#' @return Data frame without metadata characters.
#' @examples 
#' \dontrun{
#' cleanCharacters(dat)
#' }
#' @export
cleanCharacters <- function(.df) {
  .df %>% 
    mutate_each(funs(gsub("\"|@de|<|>","",.)))
}

#' Remove false results.
#' 
#' @description Removes false entries from resultset.
#' @param .df The data frame with results from DBpedia, preferably the output from function \code{\link{getConceptData}}.
#' @return Data frame with removed invalid rows.
#' @examples 
#' \dontrun{
#' filterResultSet(dat)
#' }
#' @export
filterResultSet <- function(.df) {
  .df %>% 
    filter(!grepl("index[a-z]",tolower(subject_label))) %>%
    filter(!grepl("Liste",subject_label)) %>%
    filter(!grepl("Liste",concept))
}

#' Remove paragraphs from string.
#' 
#' @description Removes all paragraphs from a string variable. Besides apply also a left and right trim of spaces. 
#' @param stringVar A character vector.
#' @return Character vector with removed paragraphs.
#' @export
noParagraph <- function(stringVar) {
  gsub('\u00c2',"",trimws(gsub("\\s+"," ",gsub("\n|\t|\r","",stringVar)),which = "both"))
}

#' Remove html-code from content
#' 
#' @description Removes html-code from a character content.
#' @param htmlContent Character vector with html-code.
#' @return Character vector with removed html-code.
#' @details This function removes unnecessary html-code from a character vector.
#' @examples 
#' \dontrun{
#' removeHtml(htmlContent)
#' }
#' @export
removeHtml <- function(htmlContent) {
  tmp <- ""
  
  if(!is.na(htmlContent)){
    localDoc <- read_html(htmlContent)
    tmp <- html_text(html_nodes(localDoc, "p"))
  }
  
  return(tmp)
}

#' Clean article data.
#' 
#' @description Cleans the article dataset. 
#' @param dat Data frame with article data, preferably the output from function \code{\link{analysePage}}.
#' @param mediaTarget  Name of the media source (e.g. "spiegelOnline").
#' @return The function returns a data frame with clean and correct formated results.
#' @details This function removes unnecessary paragraphs and character from the article result set. 
#' It also sets the right data format (e.g. date) and encoding. In addition it parse interesting data from character vectors.
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
