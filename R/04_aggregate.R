#' Describe article data.
#' 
#' @description Set of article descriptions.
#' @param .dat The object with article data, preferably the output from function \code{\link{cleanData}}.
#' @return The funtion returns a data frame with article descriptions like "number of words".
#' @details The description-columns will be added to the input data frame.
#' @export
descrArticles <- function(.dat) {
  .dat %>% 
    mutate(
      numSerchTerm      = str_count(tolower(articleContent),tolower(searchTerm)), # frequency of serchTerm in Conten
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

#' Frequency by time
#' 
#' @description Shows the number of articles by time (year, month, date etc.)
#' @param dat The object with article data, preferably the output from function \code{\link{cleanData}}.
#' @param timeDim Dimension of time in POSIX standard format.
#' @param completeDim If set to true, the time dimension will be coninuous (e.g. a month with no articles will appear in the frequency table).
#' @return Returns the frequency of elemtens and average number of words per article for the given time dimension (e.g. frequency by month) as data frame.
#' @examples
#' \dontrun{
#' freqByTime(dat,"%Y",TRUE)
#' freqByTime(dat,"%Y%m",TRUE)
#' }
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

