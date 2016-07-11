#' Describe Article Data
#' 
#' @param dat data frame with article data
#' @return data frame with additional descriptive article statistics
#' @export
descrArticles <- function(.dat) {
  .dat %>% 
    mutate(
      numSerchTerm      = str_count(tolower(articleContent),tolower(searchTerm)), # frequancy of serchTerm in Conten
      numWordsTitle     = str_count(pageHeadline,"\\s+")+1, # number of words in title
      numWordsContent   = str_count(articleContent,"\\s+")+1,
      sentenceCount     = str_count(articleContent,"\\?|\\.|\\!"), # number sentence
      searchTermInTitel = str_detect(tolower(pageHeadline),tolower(searchTerm)), #searchTerm in Title
      numParagraph      = str_count(startStringWithLetter(articleContent),"\\n[a-zA-Z]|\\t[a-zA-Z]"),
      searchTermFirstPg = str_detect(tolower(substring(startStringWithLetter(articleContent),1,
                                                       str_locate(startStringWithLetter(articleContent),"\t|\n")[,1]
                                                      )
                                             ),
                                     tolower(searchTerm)
                                     )
    ) %>% 
    mutate(
      sentenceLength    = numWordsContent/sentenceCount,
      serchTermFirstSeen= ifelse(numSerchTerm>0,
                                 str_locate(articleContent,searchTerm)[,1]/str_length(articleContent)*100,
                                 NA)
    )
}

#' aggregation: frequency by time
#' 
#' @param dat data frame with article data
#' @param timeDim dimension of time in POSIX standard format
#' @return The frequency of elemtens from the given time dimension (e.g. frequency by month) as data frame
#' @return completeDim should the time dimension be continuous (TRUE/FALSE)
#' @examples
#' freqByTime(dat,"%Y",TRUE): frequency by year
#' freqByTime(dat,"%Y%m",TRUE): frequency by year and month
#' @seealso See also \code{\link[base]{strptime}} for possible date conversions and formats.
#' @export
freqByTime <- function(dat,timeDim = "%Y",completeDim = TRUE) {
  tmp <- dat %>%
    filter(!is.na(articleDate)) %>% 
    group_by(searchTerm, 
                   timeDim = format(articleDate
                                    ,format=timeDim)) %>% 
    summarise(freq = n(),
              numWordsContent = mean(numWordsContent))
  
  if (completeDim == TRUE) {
    tmp <- unique(format(seq(min(dat$articleDate,na.rm = T),max(dat$articleDate,na.rm = T),by=1),timeDim)) %>% 
      data.frame() %>% 
      select(.,timeDim = 1) %>% 
      mutate(timeDim = as.character(timeDim)) %>% 
      left_join(.,tmp,by="timeDim") %>% 
      mutate(freq = ifelse(is.na(freq),0,freq),
             numWordsContent = ifelse(is.na(numWordsContent),0,numWordsContent)) %>% 
      data.frame
  }
  return(tmp)
}

